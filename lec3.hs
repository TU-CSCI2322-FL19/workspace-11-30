--type String = [Char]

aNum = 5
aFloat = 7.2
aString = "hello"
aList = [1,2,3]
aNotherList :: [String]
aNotherList = ["Hello", "World"]
aThirdList = [[5,9],[],[7]]
aTuple = ("Janice", 19)
aNotherTuple = (7,9,12)

type Name = String
type Schedule = [String]
students :: [(Name, Schedule)]
students = [("Janice McOverload", ["CSCI2322","CSCI3396","MATH3326","BIO3442","ENGL2401", "ART2405","PHYS1301"]),
            ("Morgan TheFifth", ["CSCI2322" ,"CSCI3396","MATH3326"])]
