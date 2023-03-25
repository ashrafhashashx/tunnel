import CodeWorld

rings :: Double -> [Picture]
rings t= [translated (2*sin ((t-i)/30)) (2*sin ((t-i)/25))
           (dilated(exp(f(tt-i))) step) |i<-[1..tt+1]]  
       where 
           f x = if x>0 then a*x else 0
           a = 0.1
           tt = 1.2*t/a
           step= colored c (circle (0.05))
           c = white

main :: IO()
main = animationOf scene
background ::Picture
background= colored black $ solidRectangle 100 100
scene :: Double -> Picture
scene t =  (pictures $ drop ((length (rings t))-60) (rings t)) & background





