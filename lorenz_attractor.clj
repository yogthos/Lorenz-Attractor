(ns lorenz-attractor
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
         (double (+ z (* dt dz)))])
      )))

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
    
    (if (not (.contentsLost buffer))
      (. buffer show))
    (.. Toolkit (getDefaultToolkit) (sync))))

(defn get-renderer [width height xscale yscale]
  (let [[xmin xmax] xscale
        [ymin ymax] yscale
        dx (- xmax xmin)
        dy (- ymax ymin)]

    ;;renderer
    (fn [g point]
      (let [[x r y] (:value point)
            xs (/  (* width (- x xmin)) dx)
            ys (/ (* height (- ymax y)) dy)
            r2 (/ r 2)]
        (.setColor g (:color point))        
        (if (> r 1)
          (.fill g (new Ellipse2D$Double (- xs r2), (- ys r2), r, r))
          (.fillRect g (Math/round xs) (Math/round ys) 1 1))
        ))))

(defn draw-lorenz [canvas renderer lorenz]  
    (draw canvas 
      (fn [g] (doseq [point lorenz] (renderer g point)))))

(defn -main [& args]
  (let [[width height] args
        frame  (JFrame. "Lorenz Attractor")
        canvas (Canvas.)
        renderer (get-renderer width height [-25, 25], [0, 50])
        update (get-update-fn 10, 28, (/ 8 3))
        dt 0.01]
    
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
    (loop [lorenz [{:color Color/RED     :value [0.0, 20.0, 20.0]}
                   {:color Color/BLUE    :value [0.3, 23.0 23.0]}
                   {:color Color/WHITE   :value [0.6, 26.0 26.0]}
                   {:color Color/GREEN   :value [0.9, 29.0 29.0]}
                   {:color Color/PINK    :value [1.2, 32.0 32.0]}
                   {:color Color/MAGENTA :value [1.5, 35.0 35.0]}
                   {:color Color/YELLOW  :value [1.8, 38.0 38.0]}]]
      (Thread/sleep 20)   
      (draw-lorenz canvas renderer lorenz)
      (recur (map #(update dt %)  lorenz)))        
    ))    

(-main 500 500)