(ns vim)
(use '[clojure.java.shell :only [sh]])

;; Global constants

;; ANSI keyboard constants
(def C-b 2)
(def C-d 4)
(def C-f 6)
(def C-n 14)
(def C-p 16)

(def KeyExit C-d)
(def KeyUp C-p)
(def KeyDown C-n)
(def KeyLeft C-b)
(def KeyRight C-f)
(def Enter 13)

(defn my-print [args]
  (let [s args]
    (.write System/out (byte-array (map byte s))))
  (.flush System/out)
  )


;; ANSI control escape seqs
(def Esc (char 27))
(def CSI (str Esc \[))
(defn csi [& args] (apply (partial str CSI) args))

;; Cursor control
(def CursorUp
  (csi (char 1) \A))
(def CursorDown
  (csi (char 1) \B))
(def CursorLeft
  (csi (char 1) \D))
(def CursorRight
  (csi (char 1) \C))
(def ClearScreen
  (str Esc \c))

;; Execute a shell command and inherit tty
(defn mysh [& args] 
  (let [pb (new ProcessBuilder args)]
    (.redirectOutput pb java.lang.ProcessBuilder$Redirect/INHERIT)
    (.redirectInput pb java.lang.ProcessBuilder$Redirect/INHERIT)
    (.redirectError pb java.lang.ProcessBuilder$Redirect/INHERIT)
    (let [p (.start pb)]
      (.waitFor p))))
(defn print-buf [buf]
  (doseq [line buf]
    (println (str line "\r"))))
(defn adjust-cur [row col]
  (my-print (csi (str (+ row 1) \; (+ col 1) \H))))

; adjust console to raw mode
(mysh "/bin/stty" "-echo" "raw")
(my-print ClearScreen)

(loop [buf ["a"], cur-row 0, cur-col 0]
  (let [input (.read System/in)]
    (my-print ClearScreen)
    (cond
      (= input KeyExit)
        (System/exit 0)
      (= input KeyUp)
        (my-print CursorUp)
      (= input KeyDown)
        (my-print CursorDown)
      (= input KeyLeft)
        (do 
          (print-buf buf)
          (adjust-cur cur-row (- cur-col 1))
          (recur buf cur-row (- cur-col 1)))
      (= input KeyRight)
        (do 
          (print-buf buf)
          (adjust-cur cur-row (+ cur-col 1))
          (recur buf cur-row (+ cur-col 1)))
      (= input Enter)
        (do
          (let [new-buf (concat buf [""]), new-row (+ cur-row 1), new-col 0]
            (print-buf new-buf)
            (adjust-cur new-row new-col)
            (recur new-buf new-row new-col)))
      :else 
        (case (char input)
          (let [line (nth buf cur-row), new-col (+ cur-col 1)]
            (let [new-line (apply str (concat (take cur-col line) (cons (char input) (take-last (- (- (count line) cur-col) 1) line))))]
              (let [new-buf (concat (take cur-row buf) (cons new-line (take-last (- (- (count  buf) cur-row) 1) buf)))]
                (print-buf new-buf)
                (adjust-cur cur-row new-col)
                (recur new-buf cur-row new-col))))))))
