minimum ([x]) = x
minimum (x:xs) = if x < minimum xs then x else minimum xs

maximum ([x]) = x
maximum (x:xs) = if x > maximum xs then x else maximum xs
