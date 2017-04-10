% it is to get shot data from masters.com 
% inWater equals 0/1/z(not exist)

clear all

% get players data
filex = 'http://www.masters.com/en_US/xml/gen/players/select.json';
playerList = webread(filex);
numElements = {playerList.players.id};
last_name = {playerList.players.last_name};
first_name = {playerList.players.first_name};
n=length(numElements);

% get holes data
filex1 = 'http://www.masters.com/en_US/xml/man/course/holes.json';
holeList = webread(filex1);
par = {holeList.holes.par};
yds = {holeList.holes.yds};

% create tables 
a=table;
g=table;
T=table;
v=table;

% get shot data from structures
for i=1:n
    % get shot data from masters.com
    filex2 = ['http://www.masters.com/en_US/xml/gen/companion/' numElements{i} '.json'];
    
    s =  webread(filex2);
    
    % round loop
    [rr, rc] = size (s.round);
    
    if rr == 5
        rr = 4;
    end
    
    for j=1:rr
          % hole loop (playoff may have less holes so cant loop to 18
          [num_holes, num_cols] = size (s.round(j).hole);
          
          for k = 1:num_holes
               % shot data
               if isstruct(s.round(j).hole(k).shot)
                    this_struct = s.round(j).hole(k).shot;
                    
                    struct_no_highlights = rmfield(this_struct, {'highlightURL' 'highlightId'});
                    
                    a = struct2table(struct_no_highlights);
                    
                    
                    % inWater equals 0/1/not exist
                    
                    [ar, ac] = size (a);
                    if ac == 16
                         a.inWater = repmat({'z'},ar,1);
                    end
               else
                    % shot data is saved into cells other than a structure
                    sr = length(s.round(j).hole(k).shot);
                    
                    for p = 1: sr
                         % disp(['p: ', num2str(p), ' is'])
                         this_struct = s.round(j).hole(k).shot{p};
                         struct_no_highlights = rmfield(this_struct, {'highlightURL' 'highlightId'});
                         
                         g = struct2table(struct_no_highlights);
                         
                        
                         % inWater equals 0/1/not exist
                         [gr, gc] = size (g);
                         
                         if gc == 16
                              g.inWater = 'z';
                         end
                      
                        
                         % array to cell
                         for q = 1: 17
                              g.(q) = cellstr(g{1,q});
                         end
                                   
                         v = vertcat(v,g);
                    end
                    a = v;
                    v=table;
               end
               % other variables
               [hr, hc] = size (a);
               a.Hole= repmat({s.round(j).hole(k).id},hr,1);
               a.Par_Value = repmat(par(k),hr,1);
               a.Yardage = repmat(yds(k),hr,1);
               a.live = repmat({s.round(j).hole(k).live},hr,1);
               a.vod = repmat({s.round(j).hole(k).vod},hr,1);
               a.bad = repmat({s.round(j).hole(k).bad},hr,1);
               a.Round = repmat({num2str(j)},hr,1); 
               a.Player_id = repmat(numElements(i),hr,1);
               a.Player_LastName = repmat(last_name(i),hr,1);
               a.Player_FirstName = repmat(first_name(i),hr,1);

               T = vertcat(T,a);  
          end
    end    
end
[tr, tc] = size (T);

% calculate hole scores
player_id = str2double(T.(24));
round_id = str2double(T.(23));
shot_id = str2double(T.num);
hole_id = str2double(T.(17));

id = [player_id round_id hole_id];
[id_unique, ia, N_long] = unique(id,'rows');
n = length(id_unique);
N = [1:n]';

shot_id_unique = unique(shot_id);
l = length(shot_id_unique);

shotN = accumarray([N_long,shot_id],1,[n l],@mean);
Score = sum(shotN,2);
score_long = interp1(N,Score,N_long);

% From_Location & To_Location

from = str2double(T.from);
[from_unique, ifrom, N_from] = unique(from);
locationCell = {'Tee Box';'Fairway';'Fairway Bunker';'Primary Rough';'Green Side Bunker';...
'Fringe';'Green';'Other';'N/A';'Native Area';'N/A'};
from_location = locationCell(N_from);
n_from = length(from_location);
to_location = cell(n_from,1);
to_location(1:n_from-1,1) = from_location(2:n_from,1);
to_location(n_from,1) = {'Green'};
indicate_from = score_long == shot_id;
to_location(indicate_from,1) = {'Green'};

% set values and outputs
T.Tourn_id = repmat({num2str(230)},tr,1); 
T.Permanent_Tournament_id = repmat({num2str(14)},tr,1);
T.Year = repmat({'2016'},tr,1); 
T.Tournament_Name = repmat({'Masters'},tr,1); 
T.Course_Name = repmat({'Augusta'},tr,1); 
T.Tour_Code = repmat({'R'},tr,1); 
T.Tour_Description = repmat({'PGA Tour'},tr,1); 
T.Hole_Score = score_long; 
T.Shot_Type = repmat({'S'},tr,1);
T.Num_of_Strokes = repmat({1},tr,1);
T.From_Location_Scorer = from_location;
T.From_Location_Laser = from_location;
T.To_Location_Scorer = to_location;
T.To_Location_Laser = to_location;
T.In_the_Hole_Flag = repmat({0},tr,1);
T.Around_the_Green_Flag = repmat({0},tr,1);
T.First_Putt_Flag = repmat({0},tr,1);
T.Distance_to_Pin = repmat({0},tr,1);
T.Time = repmat({0},tr,1);
T.Lie = repmat({0},tr,1);
T.Elevation = repmat({0},tr,1);
T.Slope = repmat({0},tr,1);
T.Z_Coordiante = repmat({0},tr,1);
T.Distance_From_Center = repmat({0},tr,1);
T.Distance_From_Edge = repmat({0},tr,1);
T.Date = repmat({0},tr,1);
T.Course_Num = repmat({14},tr,1);

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
Nr = 4;
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

T.X_Coordiante =  Txo;
T.Y_Coordiante =  Tyo;

% results

T = T(:,{'Tour_Code','Tour_Description','Year','Tourn_id','Player_id','Course_Num','Permanent_Tournament_id','Player_FirstName','Player_LastName',...
   'Round','Tournament_Name','Course_Name','Hole','Hole_Score','Par_Value','Yardage','num','Shot_Type','Num_of_Strokes','From_Location_Scorer',...
   'From_Location_Laser','To_Location_Scorer','To_Location_Laser','length','Distance_to_Pin','In_the_Hole_Flag','Around_the_Green_Flag',...
   'First_Putt_Flag','remaining','Time','Lie','Elevation','Slope','X_Coordiante','Y_Coordiante','Z_Coordiante',...
   'Distance_From_Center','Distance_From_Edge','Date','from','isGrid','x','y','z','xz','yz','zz','xm','ym','zm','ongreen','inWater','live','vod','bad'});

T.Properties.VariableNames{'num'} = 'Shot';
T.Properties.VariableNames{'length'} = 'Distance';
T.Properties.VariableNames{'remaining'} = 'Distance_to_Hole_AfterTheShot';

writetable(T,'mastersData1.txt','Delimiter',';');

