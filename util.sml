
structure Util =

struct 

fun zip3 (x::xs, y::ys, z::zs) = (x,y,z) :: zip3 (xs, ys, zs)
  | zip3 (_, _, _) = []

fun sortLabels lXList =
    let val e = foldl (fn ((l,x),e) => SEnv.insert(e,l,x))
                  SEnv.empty lXList
    in  SEnv.listItemsi e
    end

fun list x = [x]

fun intersperse sep [] = ""
  | intersperse sep [x] = x
  | intersperse sep (x::xs) = x ^ sep ^ intersperse sep xs

end