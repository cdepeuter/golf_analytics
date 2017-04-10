% it is used to save data from masters.com
clear all

url = 'http://www.masters.com/en_US/xml/gen/players/select.json';
filename = 'player_masters.txt';
outfilename = websave(filename,url);

filex = 'http://www.masters.com/en_US/xml/gen/players/select.json';
playerList = webread(filex);
numElements = {playerList.players.id};
n=length(numElements);

url1 = 'http://www.masters.com/en_US/xml/man/course/holes.json';
filename1 = 'holes.txt';
outfilename1 = websave(filename1,url1);

for i=1:n
url2 = ['http://www.masters.com/en_US/xml/gen/companion/' numElements{i} '.json'];
filename2 = ['shot_' numElements{i} '.txt'];
outfilename2 = websave(filename2,url2);
end

url3 = 'http://www.masters.com/en_US/scores/feeds/scores.json';
filename3 = 'score_masters.txt';
outfilename3 = websave(filename3,url3);

% url4 = 'http://www.masters.com/mas/js/shot_tracker.js';
% filename4 = 'shot_tracker_masters.txt';
% outfilename4 = websave(filename4,url4);

url5 = 'http://www.masters.com/en_US/xml/gen/companion/teepin.json';
filename5 = 'teepin_masters.txt';
outfilename5 = websave(filename5,url5);

url6 = 'http://www.masters.com//en_US/xml/man/video/green_video.json';
filename6 = 'green_video_masters.txt';
outfilename6 = websave(filename6,url6);

