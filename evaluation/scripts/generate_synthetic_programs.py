# %% Setup

BASE_DIR = "evaluation/input/synthetic_programs"
MAX_N = 5


def make_elm(N):
    ifs = ("if p hd then " * N).strip()
    elses = ("else main p tl " * N).strip()
    return f"""main : (Int -> Bool) -> List Int -> List Int
main p xs =
  case xs of
    [] -> []
    hd :: tl ->
      {ifs}
        hd :: main p tl
      {elses}"""


def make_python(N):
    rhs = "x[i]"
    for i in range(N):
        op = "+" if i % 2 == 0 else "*"
        rhs += f" {op} x[i]"
    return f"""ret = np.zeros(len(x))
for i in range(len(x)):
    ret[i] = {rhs}
ret"""


# %% Main

for N in range(1, MAX_N + 1):
    with open(f"{BASE_DIR}/elm{N}.elm", "w") as f:
        f.write(make_elm(N))
    with open(f"{BASE_DIR}/python{N}.py", "w") as f:
        f.write(make_python(N))
