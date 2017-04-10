fname ='shot_34431.txt';
fid = fopen(fname);
raw = fread(fid);
str = char(raw');
fclose(fid);

data1 = parse_json(str)
data2 = JSON.parse(str)
data3 = loadjson('shot_34431.txt');