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
          dz (- (* x y) (* beta (/ z 3)))]
      (assoc lorenz 
        :value        
        [(+ x (* dt dx))  
         (+ y (* dt dy)) 
         (+ z (* dt dz))]))
    ))

;;;;;;;UI;;;;;;;;; 
(defn draw [#^Canvas canvas draw-fn]
  (let [buffer  (.getBufferStrategy canvas)
        g      (.getDrawGraphics buffer)]
    
    (try
      (draw-fn g)      
      (finally (.dispose g)))
    
    (if (not (.contentsLost buffer))
      (. buffer show))
    (.. Toolkit (getDefaultToolkit) (sync))))

(defn get-renderer [width height xscale yscale]
  (let [[x1 x2] xscale
        [y1 y2] yscale
        xsize (* (- x2 x1) 0.05)
        ysize (* (- y2 y1) 0.05)
        xmin (- x1 xsize)
        xmax (+ x2 xsize)
        ymin (- y1 ysize)
        ymax (+ y2 ysize)
        dx (- xmax xmin)
        dy (- ymax ymin)]
    
    (defn scale-x [x] 
      (/  (* width (- x xmin)) dx))

    (defn scale-y [y]      
      (/ (* height (- ymax y)) dy))
    
    ;;renderer
    (fn [g point]
      (let [[x z y] (:value point)
            xs (scale-x x)
            ys (scale-y y)
            r z
            r2 (/ r 2)]
        (.setColor g (:color point))        
        (if (> r 1)
          (.fill g (new Ellipse2D$Double (- xs r2), (- ys r2), r, r))
          (.fillRect g (Math/round xs) (Math/round ys) 1 1))
        ))))

(defn draw-lorenz [canvas renderer lorenz]  
    (draw 
      canvas 
      (fn [g] (doseq [point lorenz] (renderer g point)))))

(defn -main [& args]
  (let [[width height] args
        frame  (JFrame. "Lorenz Attractor")
        canvas (Canvas.)
        renderer (get-renderer width height [-25, 25], [0, 50])
        update (get-update-fn 10, 28, 8)
        dt 0.001]
    (doto frame
      (.setSize width height)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setResizable false)
      (.add canvas)
      (.setVisible true))
    
    (doto canvas
      (.createBufferStrategy 2)      
      (.setVisible true)
      (.requestFocus))
    
    ;;main loop
    (loop [lorenz [{:color Color/RED :value [0.0, 20.0 25.0]}
                   {:color Color/BLUE :value [0.1, 21.0 23.0]}]]
            
        (draw-lorenz canvas renderer lorenz)
        (recur (map #(update dt %)  lorenz)))        
    ))    

(-main 500 500)