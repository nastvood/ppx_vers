type%vers ta =
  | A1
  | A2

type%vers[@novers] tn =
  | B1
  | B2 of ta

type%vers[@novers] tn1 =
  | C1
  | C2 of tn [@from_novers]
    and tn1 = 
  | C1 of int [@migrate ? Prev.C1 when C1 0]
  | C2 of tn
