describeShape :: Int -> String 
describeShape 3 =  "trianagle"
describeShape 4 = "quadraltiral"
describeShape n = show n ++ "-sided shape"


fst' (x, _) = x 
snd' (_, x) = x 

