import com.sksamuel.scrimage.{Pixel, X11Colorlist}

val arr = "010013,,1,1".split(",")
arr.length


val a = new Array[Pixel](100)
a(0) = Pixel(1,2,3,4)
a

val black = Pixel(X11Colorlist.Black.toInt)
black.alpha