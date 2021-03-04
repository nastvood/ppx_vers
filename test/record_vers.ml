type%vers tr = {
  f1: int;
  f2: string
}

type%vers trv = {
  f1: int;
  f2: string
} and trv = {
  f1: int;
  f2: string;
  f3: float [@migrate 0.0]
} and trv = {
  f1: int;
  f2: int; [@migrate int_of_string p.Prev.f2]
  f3: float
}

type%vers trv_v = {
  fv1: int;
  fv2: string
} and trv_v = {
  fv1: int;
  fv2: string;
  fv3: trv  [@migrate {f1 = 0; f2 = 0; f3 = 0.0}]
}
