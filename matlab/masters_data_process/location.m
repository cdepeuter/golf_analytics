xsp = [699773.538, 700932.938];
ysp = [1274788.013,1273986.000];
[lon1,lat1] = sp_proj('1001','inverse',xsp,ysp,'sf');

filex = 'http://www.masters.com/en_US/xml/gen/companion/teepin.json';
teepinList = webread(filex);

a= struct2table(teepinList.round(1).tees)
b = a.x
c=a.xo
d=interp1(tmp1, tmp3, tmp11)
e= a.y
 tmp1 = [];
for i = 1:size(b,1)
tmp1 = [tmp1 str2double(b{i})];
end

tmp3 = [];
for i = 1:size(c,1)
tmp3 = [tmp3 str2double(c{i})];
end

tmp2 = [];
for i = 1:size(e,1)
tmp2 = [tmp2 str2double(e{i})];
end

a1= struct2table(teepinList.round(2).tees)
b1 = a1.x
c1=a1.xo
d=interp1(tmp1, tmp3, tmp2)
e1= a1.y
 tmp11 = [];
for i = 1:size(b1,1)
tmp11 = [tmp11 str2double(b1{i})];
end

tmp31 = [];
for i = 1:size(c1,1)
tmp31 = [tmp31 str2double(c1{i})];
end

tmp21 = [];
for i = 1:size(e1,1)
tmp21 = [tmp21 str2double(e1{i})];
end