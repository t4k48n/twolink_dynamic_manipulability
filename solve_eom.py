from sympy import *

# 分数1/2
f12 = Rational(1, 2)

# 時間
t = var("t", negative=False)

# リンク1, 2の角度
q1 = Function("q1", real=True)(t)
q2 = Function("q2", real=True)(t)

# リンク1, 2, 把持物3の質量
m1 = var("m1", positive=True)
m2 = var("m2", positive=True)
m3 = var("m3", positive=True)

# リンク長さ
l1 = var("l1", positive=True)
l2 = var("l2", positive=True)

# 慣性モーメント
I1 = var("I1", positive=True)
I2 = var("I2", positive=True)

# 重心位置
x1 = f12 * l1 * cos(q1)
x2 = l1 * cos(q1) + f12 * l2 * cos(q1 + q2)
x3 = l1 * cos(q1) + l2 * cos(q1 + q2)
y1 = f12 * l1 * sin(q1)
y2 = l1 * sin(q1) + f12 * l2 * sin(q1 + q2)
y3 = l1 * sin(q1) + l2 * sin(q1 + q2)
# x1, x2, x3, y1, y2, y3の時間微分（1階）
dx1 = x1.diff(t)
dx2 = x2.diff(t)
dx3 = x3.diff(t)
dy1 = y1.diff(t)
dy2 = y2.diff(t)
dy3 = y3.diff(t)

# 絶対角度（x軸基準）
th1 = q1
th2 = q1 + q2
# th1, th2の時間微分（1階）
dth1 = th1.diff(t)
dth2 = th2.diff(t)

# 運動エネルギー
K1 = trigsimp(f12 * m1 * dx1 ** 2 + f12 * m1 * dy1 ** 2 + f12 * I1 * dth1 ** 2)
K2 = trigsimp(f12 * m2 * dx2 ** 2 + f12 * m2 * dy2 ** 2 + f12 * I2 * dth2 ** 2)
K3 = trigsimp(f12 * m3 * dx3 ** 2 + f12 * m3 * dy3 ** 2)
K = K1 + K2 + K3

# 重力加速度
g = var("g", negative=False)

# ポテンシャルエネルギー
U1 = m1 * g * y1
U2 = m2 * g * y2
U3 = m3 * g * y3
U = U1 + U2 + U3

# ラグランジアン
L = K - U

# 角度の時間微分（1階、2階）
dq1 = q1.diff(t)
dq2 = q2.diff(t)
ddq1 = dq1.diff(t)
ddq2 = dq2.diff(t)

# ラグランジュ形式の運動方程式の左辺
lhs1 = (L.diff(dq1).diff(t) - L.diff(q1)).expand().collect(g).collect(ddq1).collect(ddq2).collect(dq1).collect(dq2)
lhs2 = (L.diff(dq2).diff(t) - L.diff(q2)).expand().collect(g).collect(ddq1).collect(ddq2).collect(dq1).collect(dq2)

# M [ddq1 ddq2] + H + G = T（運動方程式の行列形式）
# 慣性行列（加速度項）
M = [sum([a.subs(ddq1, 1) for a in lhs1.args if ddq1 in a.atoms(Derivative, Symbol)]), sum([a.subs(ddq2, 1) for a in lhs1.args if ddq2 in a.atoms(Derivative, Symbol)]),
     sum([a.subs(ddq1, 1) for a in lhs2.args if ddq1 in a.atoms(Derivative, Symbol)]), sum([a.subs(ddq2, 1) for a in lhs2.args if ddq2 in a.atoms(Derivative, Symbol)])]
# 速度項
H = [sum([a for a in lhs1.args if dq1 in a.atoms(Derivative, Symbol) or dq2 in a.atoms(Derivative, Symbol)]),
     sum([a for a in lhs2.args if dq1 in a.atoms(Derivative, Symbol) or dq2 in a.atoms(Derivative, Symbol)])]
# ポテンシャル項
G = [sum([a for a in lhs1.args if g in a.atoms(Derivative, Symbol)]),
     sum([a for a in lhs2.args if g in a.atoms(Derivative, Symbol)])]
# トルク
T = [Symbol("tau1"),
     Symbol("tau2")]

# OCamlで使うために整形してプリントする
m11, m12, m21, m22 = [str(m).replace("q1(t)", "q1").replace("q2(t)", "q2").replace("*", " *. ").replace("*.  *.", "**").replace("/", " /. ").replace("+", "+.").replace(" 2", " 2.0").replace(" 4", " 4.0").replace("I1", "inert1").replace("I2", "inert2").replace("m1", "m1'").replace("m2", "m2'").replace("m3", "m3'").replace("l1", "l1'").replace("l2", "l2'").replace("[", "(").replace("]", ")") for m in M]
print("let m11 = {} in".format(m11))
print("let m12 = {} in".format(m12))
print("let m21 = {} in".format(m21))
print("let m22 = {} in".format(m22))
