

module Module1 = struct

  type%vers[@novers] t = {
    f1: int;
    f2: string;
  } and t = {
    f1: int;
    f2: string;
    f4: float [@migrate 0.0]
  }

end  

module ModuleFake = struct

  type t = int

end

module Module2 = struct

  type%vers[@novers] t = 
    | A1
    | A2 of Module1.t [@from_novers]

end  

