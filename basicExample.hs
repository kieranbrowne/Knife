import Knife

main = print (map toRGB (cool . complementary $ getStock "wheat" ))
