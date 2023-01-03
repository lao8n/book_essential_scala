object Union {
    def Union(x: Set[A], y: Set[A]): Set[A] = {
        y.foldLeft(x){
            (set, ele) => set + ele
        }
    }

    def Map(x: Map[A, B], y: Map[A, B]): Map[A, B] = {
        y.foldLeft(x) {
            (xMap, yEle) => 
                val (yk, yv) = yEle
                val xv = xMap.get(ky)
                xMap + (yk -> yv + xv.getOrElse(0))
        }     
    }

    def MapGeneric(x: Map[A, B], y: Map[A, B], add: (B, B) => B): Map[A, B] = {
        y.foldLeft(x) {
            (xMap, yEle) => 
                val (yk, yv) = yEle
                val newV = xMap.get(yk).map(xv => add(yv, xv)).getOrElse(yv)
                xMap + (yk -> newV)
        }
    }
}
