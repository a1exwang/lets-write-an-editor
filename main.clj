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

; adjust console to raw mode
(mysh "/bin/stty" "-echo" "raw")

(my-print ClearScreen)

(loop []
  (let [input (.read System/in)]
    (cond
      (= input KeyExit)
        (System/exit 0)
      (= input KeyUp)
        (my-print CursorUp)
      (= input KeyDown)
        (my-print CursorDown)
      (= input KeyLeft)
        (my-print CursorLeft)
      (= input KeyRight)
        (my-print CursorRight)
      :else 
        (my-print (str (char input)))
      ))
  (recur))
