(ns multi_snake.core
  (:import [java.awt Color Dimension]
	   [javax.swing JPanel JFrame Timer JOptionPane SwingUtilities UIManager] 
           [java.awt.event ActionListener KeyListener]
           [javax.imageio ImageIO]
           [java.io File])
  (:gen-class :main true))

;(set! *warn-on-reflection* true)

(defn- set-laf! "Set look and feel of the ui, provided its name as a string."  
[laf-name]
(when-let [lf1 (some #(when (= laf-name (.getName  %)) %) (UIManager/getInstalledLookAndFeels))]
  (UIManager/setLookAndFeel ^String (.getClassName  lf1))))

(set-laf! "Nimbus")    

; ----------------------------------------------------------
; functional model
; ----------------------------------------------------------
(def width 75)
(def height 50)
(def point-size 10)
(def ^Dimension panel-dimension (Dimension. (* (inc width)  point-size) 
		                            (* (inc height) point-size)))		                            
(def turn-millis 100)
(def win-length 100)
(def timer (atom nil))
(def directions {:UP    [ 0 -1]
                 :DOWN  [ 0  1]
                 :LEFT  [-1  0]
                 :RIGHT [ 1  0]})
(def dirs {java.awt.event.KeyEvent/VK_LEFT  (with-meta (:LEFT directions) {:snake 1})
           java.awt.event.KeyEvent/VK_RIGHT (with-meta (:RIGHT directions) {:snake 1})
           java.awt.event.KeyEvent/VK_UP    (with-meta (:UP directions) {:snake 1})
	   java.awt.event.KeyEvent/VK_DOWN  (with-meta (:DOWN directions) {:snake 1})
	   java.awt.event.KeyEvent/VK_A     (with-meta (:LEFT directions) {:snake 2}) 
           java.awt.event.KeyEvent/VK_D     (with-meta (:RIGHT directions) {:snake 2})
           java.awt.event.KeyEvent/VK_W     (with-meta (:UP directions) {:snake 2})
	   java.awt.event.KeyEvent/VK_S     (with-meta (:DOWN directions) {:snake 2})})	   

(defn add-points [& pts]
 (apply mapv + pts))
 
(defn- make-image 
"Returns a buffered-image from the specified file or nil if the file is not there." 
[^java.net.URL url]
(try  (ImageIO/read url)   
(catch java.io.IOException e ;returning nil here  
  (println url "does not exist! Reverting to nil...")))) 
  
(defonce apple-image (-> "images/apple.png" 
                        clojure.java.io/resource
                        make-image)) 

(defn point-to-screen-rect [pt] 
  (mapv #(* point-size %) 
       [(first pt) (second pt) 1 1]))

(defn create-apple [] 
  {:location (vector (rand-int width) (rand-int height))    
   ;:color Color/RED
   :image apple-image 
   :type :apple}) 

(defn create-snake [[x y :as start-pos] ^Color color]
  {:body (list [(dec x) y] start-pos ) 
   :dir [1 0]
   :type :snake
   :color color})

(defn move [{:keys [body dir] :as snake} & grow]
  (assoc snake :body (cons (add-points (first body) dir) 
			   (if grow body (butlast body)))))
			   
(defn opposite? [[x1 y1] [x2 y2]]
 (and (zero? (+ x1 x2)) 
      (zero? (+ y1 y2))))
 
(defn turn [snake newdir]
 (if (or (opposite? newdir (:dir snake))
         (= newdir (:dir snake))) snake  
 (assoc snake :dir newdir))) 

(defn win? [{body :body}]
  (>= (count body) win-length))

(defn head-overlaps-body? [{[head & body] :body}]
  (contains? (set body) head))
  
(defn hit-border? [{[head & body] :body}] 
 (let [[x y & more] (point-to-screen-rect head)
       xlimit (.getWidth  panel-dimension)
       ylimit (.getHeight panel-dimension)]
  (or  (or (>= x  xlimit)
           (neg? x))
       (or (>= y ylimit)
           (neg? y)))))
           
(defn crashes? [{[head1 & body1] :body} s2] 
  (contains? (set (:body s2)) head1))           
 
(defn lose? [snake]
  (or (head-overlaps-body? snake)
      (hit-border? snake)))

(defn eats? [{[snake-head] :body} {apple :location}]
   (= snake-head apple))

; ----------------------------------------------------------
; mutable model
; ----------------------------------------------------------
(defn update-positions [snake1 snake2 apple]
  (dosync
   (let [s1eats? (eats? @snake1 @apple)
         s2eats? (when snake2 (eats? @snake2 @apple))] 
   (if (or s1eats? s2eats?)
     (do (ref-set apple (create-apple))
         (if s1eats? 
	   (alter snake1 move :grow)
	   (alter snake2 move :grow))
	   (swap! timer #(doto ^Timer % (.setDelay  (/ (.getDelay ^Timer %) 1.05))))  )
    (do (alter snake1 move) 
        (when snake2 (alter snake2 move))))))
  nil)

(defn update-direction [snake1 snake2 newdir]
  (when newdir
    (if (= 1 (-> newdir meta :snake)) 
      (dosync (alter snake1 turn newdir))
      (dosync (alter snake2 turn newdir)))))

(defn reset-game [snake1 snake2 apple]
  (dosync (ref-set apple  (create-apple))
	  (ref-set snake1 (create-snake [1 1] Color/GREEN))
	  (when snake2 (ref-set snake2 (create-snake [1 20] Color/ORANGE)))
	  (swap! timer #(doto ^Timer % (.setDelay turn-millis))))
  nil)


; ----------------------------------------------------------
; gui
; ----------------------------------------------------------
(defn fill-point [^java.awt.Graphics g pt color] 
  (let [[x y width height] (point-to-screen-rect pt)]
    (.setColor g color) 
    (.fill3DRect g x y width height true)))

(defn draw-apple [^java.awt.Graphics g pt image] 
  (let [[x y width height] (point-to-screen-rect pt)]
    (.drawImage g image x y nil)))

(defmulti paint (fn [g object & _] (:type object)))

(defmethod paint :apple [g {:keys [location image]}] 
  (draw-apple g location image))

(defmethod paint :snake [g {:keys [body color]}]
  (doseq [point body]
    (fill-point g point color)))

(defn game-panel [frame snake1 snake2 apple] 
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [^java.awt.Graphics g]
      (let [^JPanel this this] 
      (proxy-super paintComponent g))
      (paint g @snake1)
      (when snake2 (paint g @snake2))
      (paint g @apple))
    (actionPerformed [^java.awt.event.KeyEvent e]
    (when (.isRunning ^Timer @timer) 
      (update-positions snake1 snake2 apple)
      (if snake2
        (do  (when (lose? @snake1)	       
	       (JOptionPane/showMessageDialog frame (str "Green snake loses!\nGreen Score: " (-> @snake1 :body count (- 2)) 
	                                                 " apples\nOrange score: "  (-> @snake2 :body count (- 2)) " apples"))
	       (reset-game snake1 snake2 apple))
	     (when (lose? @snake2)
	       (JOptionPane/showMessageDialog frame (str "Orange snake loses!\nGreen Score: " (-> @snake1 :body count (- 2)) 
	                                                 " apples\nOrange score: " (-> @snake2 :body count (- 2)) " apples"))
	       (reset-game snake1 snake2 apple))
	     (when (win? @snake1)
	       (JOptionPane/showMessageDialog frame (str "Green snake wins!\nGreen Score: " (-> @snake1 :body count (- 2)) 
	                                                 " apples\nOrange score: " (-> @snake2 :body count (- 2)) " apples"))
	        (reset-game snake1 snake2 apple))
	     (when (win? @snake2)
	       (JOptionPane/showMessageDialog frame (str "Orange snake wins!\nGreen Score: " (-> @snake1 :body count (- 2)) 
	                                                 " apples\nOrange score: " (-> @snake2 :body count (- 2)) " apples"))
	       (reset-game snake1 snake2 apple))
	     (when (crashes? @snake1 @snake2)
	       (JOptionPane/showMessageDialog frame (str "Orange snake wins!\nGreen score: " (-> @snake1 :body count (- 2)) 
	                                                 " apples\nOrange score: " (-> @snake2 :body count (- 2)) " apples"))
	       (reset-game snake1 snake2 apple))
             (when (crashes? @snake2 @snake1)
	       (JOptionPane/showMessageDialog frame (str "Green snake wins!\nGreen score: " (-> @snake1 :body count (- 2)) 
	                                                 " apples\nOrange score: " (-> @snake2 :body count (- 2)) " apples"))
	       (reset-game snake1 snake2 apple)) )       
        (do  (when (lose? @snake1)
	      (JOptionPane/showMessageDialog frame (str "Green snake loses!\nScore: " (-> @snake1 :body count (- 2)) " apples"))
	      (reset-game snake1 snake2 apple))    
	    (when (win? @snake1)
	      (JOptionPane/showMessageDialog frame (str "Green snake wins!\nScore: " (-> @snake1 :body count (- 2)) " apples"))
	      (reset-game snake1 snake2 apple)) ))
      (.repaint ^JPanel this)))
    (keyPressed [^java.awt.event.KeyEvent e]
      (let [code (.getKeyCode e)]
        (if (= code java.awt.event.KeyEvent/VK_SPACE)
          (if (.isRunning ^Timer @timer)  (.stop ^Timer @timer)
            (.start ^Timer @timer))
          (when-let [c (get dirs code)]
           (try  (update-direction snake1 snake2 c) 
           (catch  NullPointerException _ nil)))))) ;;in case snake2 is nil and the user pressed [W,A,S,D] for some reason
    (getPreferredSize [] panel-dimension)
    (keyReleased [^java.awt.event.KeyEvent e])
    (keyTyped [^java.awt.event.KeyEvent e])))

(defn- game [snake-number]
 (assert (or (= 1 snake-number) 
             (= 2 snake-number)) "1 or 2 snakes supported!") 
  (let [snake1 (ref   (create-snake [2 1]  Color/GREEN)) 
        snake2 (when (= 2 snake-number) 
                 (ref (create-snake [2 20] Color/ORANGE)))
	apple (ref (create-apple))
        frame (JFrame. "Multi-Snake")
      ^JPanel	panel (game-panel frame snake1 snake2 apple)
      ^Timer t (reset! timer (Timer. turn-millis panel))]
    (doto panel 
      (.setFocusable true)
      (.setBackground Color/LIGHT_GRAY)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      ;;(.setIconImage (ImageIO/read (File. "resources/images/xxx.png")))
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.pack)
      (.setLocationRelativeTo nil)
      (.setVisible true))
    (.start t)
    [snake1, snake2, apple, t])) 
    
(defn- welcome []
 (let [options (to-array ["1 Player" "2 Players"])]
   (JOptionPane/showOptionDialog nil "How many players?" "New Game" 
    JOptionPane/YES_NO_OPTION 
    JOptionPane/QUESTION_MESSAGE nil options (first options))))    

(defn -main [& args]
  (let [options (to-array ["1 Player" "2 Players"])
        answer (welcome)    
        players (inc answer)]
    (SwingUtilities/invokeLater #(game players))))
       
    
   


