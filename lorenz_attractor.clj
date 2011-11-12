(ns lorenz-attractor
  (:use util)
  (:import
    (javax.swing JFrame)
    (java.awt.geom Ellipse2D$Double)
    (java.awt Canvas Color Toolkit)))
     
(defn get-update-fn [prandtl, rayleigh, beta]
  (fn [dt, lorenz]
    (let [[x y z] (:value lorenz)
          dx (* (- prandtl) (- x y))
          dy (- (+ (- (* rayleigh x) (* x z))) y)
          dz (- (* x y) (* beta z))]
      (assoc lorenz 
        :value        
        [(double (+ x (* dt dx)))  
         (double (+ y (* dt dy))) 
         (double (+ z (* dt dz)))]))))

;;;;;;;UI;;;;;;;;; 
(defn draw [#^Canvas canvas draw-fn]
  (let [buffer (.getBufferStrategy canvas)
        g      (.getDrawGraphics buffer)]    
    (try
      (doto g
        (.setColor Color/BLACK)
        (.fillRect 0 0 (.getWidth canvas) (.getHeight canvas)))
      (draw-fn g)      
      (finally (.dispose g)))
    
    (when-not (.contentsLost buffer)
      (. buffer show))
    (.. Toolkit (getDefaultToolkit) (sync))))

(defn get-renderer [width height [xmin xmax] [ymin ymax]]
  (let [dx (- xmax xmin)
        dy (- ymax ymin)]
    
    ;;renderer
    (fn [g {[x r y] :value, color :color}]
      (let [xs (/  (* width (- x xmin)) dx)
            ys (/ (* height (- ymax y)) dy)
            r2 (/ r 2)]
        (.setColor g color)        
        (if (> r 1)
          (.fill g (new Ellipse2D$Double (- xs r2), (- ys r2), r, r))
          (.fillRect g (Math/round xs) (Math/round ys) 1 1))))))

(defn draw-lorenz [canvas renderer lorenz]  
    (draw canvas 
      (fn [g] (doseq [point lorenz] (renderer g point)))))

(defn -main [& args]
  (let [[width height] args
        frame          (JFrame. "Lorenz Attractor")
        canvas         (Canvas.)
        renderer       (get-renderer width height [-25, 25], [0, 50])
        update         (get-update-fn 10, 28, (/ 8 3))
        dt             0.01]
    
    (doto frame
      (.setSize width height)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setResizable false)
      (.add canvas)
      (.setVisible true))
    
    (doto canvas
      (.createBufferStrategy 2)            
      (.requestFocus))
    
    ;;main loop
    (loop [lorenz (take 30
                    (map (fn [color value] {:color color :value value})
                         (cycle [Color/RED, Color/BLUE, Color/WHITE, Color/GREEN, Color/YELLOW]) 
                         (iterate (fn [col] [(rand 30), (rand 30), (rand 30)]) [0.0, 0.0, 0.0])))]
                      
      (Thread/sleep 20)   
      (draw-lorenz canvas renderer lorenz)
      (recur (map #(update dt %)  lorenz)))        
    ))    

(-main 500 500)
