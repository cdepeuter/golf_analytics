% to tansform from course coordinate systems to an earth coordinate system
filexTP = 'http://www.masters.com/en_US/xml/gen/companion/teepin.json';
teepin = webread(filexTP);
round = 4;
Ttx = [];
Tpx = [];
Ttxo = [];
Tpxo = [];
Tty = [];
Tpy = [];
Ttyo = [];
Tpyo = [];

for i = 1:4;
    Tt = struct2table(teepin.round(i).tees);
    Ttx = [Ttx; str2double(Tt.x)];
    Ttxo = [Ttxo; str2double(Tt.xo)];
    Tty = [Tty; str2double(Tt.y)];
    Ttyo = [Ttyo; str2double(Tt.yo)];
    Tp = struct2table(teepin.round(i).pins);
    Tpx = [Tpx; str2double(Tp.x)];
    Tpxo = [Tpxo; str2double(Tp.xo)];
    Tpy = [Tpy; str2double(Tp.y)];
    Tpyo = [Tpyo; str2double(Tp.yo)];
end

l = length (Tpx);
for j=1:l
    syms a b c d
    eqn1 = Ttx(j)*a + b == Ttxo(j);
    eqn2 = c + Tty(j)*d == Ttyo(j);
    eqn3 = Tpx(j)*a + b == Tpxo(j);
    eqn4 = c + Tpy(j)*d == Tpyo(j);
    [x1, x2, x3, x4] = solve([eqn1, eqn2, eqn3, eqn4], [a,b,c,d]);
    A(j) = double(x1);
    B(j) = double(x2);
    C(j) = double(x3);
    D(j) = double(x4);
end

Tsx = str2double(T.x);
Tsy = str2double(T.y);
Tr = str2double(T.Round);
Th = str2double(T.Hole);
Nr = length(unique(Tr));
Nh = length(unique(Th));

Q=0;
Txo = [];
Tyo = [];
for K = 1 : Nr
    for P = 1:Nh
        Q=Q+1;
        Index_c = Tr == K;
        Index_h = Th == P;
        Tsx_c = Tsx(Index_c & Index_h);
        Tsy_c = Tsy(Index_c & Index_h);
       
        Txoi = Tsx_c*A(Q) + B(Q);
        Tyoi = C(Q) + Tsy_c*D(Q); 
        Txo = [Txo; Txoi];
        Tyo = [Tyo; Tyoi];
    end
end


