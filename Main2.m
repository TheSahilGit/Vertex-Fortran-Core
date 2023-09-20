
clear;clc;close all;
L = 20;

    
israndom = true;
%israndom = false; 
%isseed=true;
isseed = false;

    
seed = 10; % Change this if you want a different random structure.

[v,c,p] = MeshGenerator(L,israndom,isseed,seed) ;

v(1,:)=99999;


%inn(:,1:end)

%
[inn,num] = StoreData(L,v,c);
num  = num';


inn(:, end+10) = 0; % Extending the dimension of n because in fortran array dimension is static.


etasmx = 0.1;
etasmin = 0.1;
intrval = ((etasmx - etasmin)/L);

for iij = 1 : L
    for i=1:L:L*L-L+1
        etas(i+iij-1, (1:num(i+iij-1))) = etasmx - (iij-1)*intrval; 
    end 
end 

etas(:, end+size(inn,2)-size(etas,2)) = 0;    % Just being lazy. No reason to do this shi!
etas = repmat(etas, 9, 1);
%etas = etas*0;

%%-- Structuring the motility to look like fxx and fyy. So that while using
%%-- it, it does not over add at each vertex. 

etaR = zeros(length(v(:,1)),1);

for i = 1:L*L
    etaR(inn(i, 1:num(i))) = etas(i,1:num(i));
end



%% Neighbor cells.
for ii=1:L*L
    for jj =1:num(ii)
        pk = inn(ii,jj);
        ccn = 0;
        for kk = 1:L*L
            hh = find(pk==inn(kk, 1:num(kk)),1);
            if hh~=0
                ccn = ccn + 1;
            else
                ccn = ccn + 0;
            end
        end
        cono(ii,jj) = ccn;
    end
end

kk = 1;
for ii = 1:L*L
    for jj = 1:num(ii)
        if cono(ii,jj) < 3
            cellI(kk) = ii;
            verI(kk)  = jj;
            kk = kk + 1;
        end 
    end 
end 
borderver = [cellI' verI'];

para = [L;length(num); length(v(1,:)); length(v(:,1));length(inn(1,:));length(inn(:,1)); length(borderver)];

%%
    mainarea=(1:L*L);
    leftpanel=(1:L);
    rightpanel=(L*L-L+1:L*L-L+L);
    toppanel=(L:L:L*L);
    bottompanel=(1:L:L*L-L+1);
    corners=[1 L L*L-L+1 L*L ];
    
    leftpanel2=(L+1:2*L);
    rightpanel2=(L*L-2*L+1:L*L-2*L+L);
    toppanel2=(L-1:L:L*L-1);
    bottompanel2=(2:L:L*L-L+2);
   % corners2=[1 L L*L-L+1 L*L ];


    Boundary=[leftpanel rightpanel toppanel bottompanel];

    Boundary2 =[Boundary leftpanel2 rightpanel2 toppanel2 bottompanel2];

   
   
    inside1 = setdiff(mainarea,Boundary2);
    

%%


writematrix(v,'v_in.dat','Delimiter',' ');
writematrix(num, 'num_in.dat');
writematrix(inn,'inn_in.dat','Delimiter',' ');
writematrix(para, 'para2_in.dat');
writematrix(borderver, "Borderver_in.dat");
writematrix(etaR, "motility_in.dat");
writematrix(inside1', "inside_in.dat");
%% Functions
    
%%
function [v,c,p]=MeshGenerator(L,israndom,isseed,seed)
m=(1:1:L);
x=repmat(m,L,1);
%x=randi(L,L);
%x=x*sqrt(3)/2;
y = repmat(m, L, 1)';
mm = repmat([0 0.5],[L,L/2]);
y=y+mm;
y=y(:);
x=x(:);

if isseed==true
    rng(seed);
end

ran=rand(1,length(x));
if israndom==true
    x = x(:) ;
    y = y(:) ;
    
    mainarea=(1:L*L);
    leftpanel=(1:L);
    rightpanel=(L*L-L+1:L*L);
    toppanel=(L:L:L*L);
    bottompanel=(1:L:L*L-L+1);
    corners=[1 L L*L-L+1 L*L ];
    
    leftpanel2=(L+1:2*L);
    rightpanel2=(L*L-2*L+1:L*L-2*L+L);
    toppanel2=(L-1:L:L*L-1);
    bottompanel2=(2:L:L*L-L+2);
    corners2=[1 L L*L-L+1 L*L ];


    leftpanel3=(2*L+1:3*L);
    rightpanel3=(L*L-3*L+1:L*L-3*L+L);
    toppanel3=(L-2:L:L*L-2);
    bottompanel3=(3:L:L*L-L+3);
   % corners2=[1 L L*L-L+1 L*L ];

    Boundary=[leftpanel rightpanel toppanel bottompanel];

    Boundary2 =[Boundary leftpanel2 rightpanel2 toppanel2 bottompanel2]; 
   
    Boundary3 =[Boundary2 leftpanel3 rightpanel3 toppanel3 bottompanel3];

    inside2 = setdiff(mainarea,Boundary3);
   

    

    x(inside2') = x(inside2') + 0.5 * (2*ran(inside2')'-1);

    y(inside2') = y(inside2') + 0.5 * (2*ran(inside2')'-1);



    %     for i=2:2:length(x)
    %         x(i)=x(i)+0.5;
    %     end
    %
    %     for i=4:4:length(x)
    %         x(i)=x(i)+0.25;
    %         y(i)=y(i)+0.25;
    %     end

    % y=y(:)+ran(:);
end


% x
% y


xx=[x;x+L;x-L;x;x;x+L;x-L;x+L;x-L];
yy=[y;y;y;y+L;y-L;y+L;y-L;y-L;y+L];
%size(xx)
%[vx,vy]=voronoi(xx,yy);
p=[xx,yy];
%size(p)
[v,c]=voronoin(p);
end
%%
%%
function [orient,signed_area] = polyorient(x,y)
%POLYORIENT Orientation of polygon
%   Returns the orientation and signed area of a 2D polygon
%
%   Syntax:
%      [ORIENT,SAREA] = POLYORIENT(X,Y)
%
%   Inputs:
%      X, Y   Vectors with the polygon vertices
%
%   Outputs:
%      ORIENT   Polygon orientation. 1 if the orientation is
%               counter-clockwise (direct), 0 otherwise
%      SAREA    Signed area of the polygon, negative if orientation is
%               not direct
%
%   Examples:
%      x1 = [0 0 1 1]; y1 = [1 2 2 1];
%      x2 = [0 0 1 1]; y2 = [1 0 0 1];
%      x3 = [x1 x2];   y3 = [y1 y2];
%
%      [o1,a1] = polyorient(x1,y1) % 0, -1
%      [o2,a2] = polyorient(x2,y2) % 1,  1
%      [o3,a3] = polyorient(x3,y3) % 0,  0
%
%   MMA 21-4-2006, mma@odyle.net

signed_area=0.5* sum(x.*y([2:end 1]) - y.*x([2:end 1]));
orient = signed_area > 0;
end
%%
function [inn,num]=StoreData(L,v,c)

for i=1:9*L*L
    cellCoord=c{i};
    for j=1:length(cellCoord)
        inn(i,j)=cellCoord(j);
        num(i)=length(cellCoord);
    end
end
%%
% for i=L*L+1:L*L+L
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
% for i=4*L*L+L:L:5*L*L
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
% for i=3*L*L-L+1:3*L*L
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
% %
% for i=3*L*L+1:L:4*L*L
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
% for i=5*L*L+1
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
% for i=7*L*L
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
% for i=7*L*L+L
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
% %
% for i=9*L*L-L+1
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
%% Outside layer of the first ghost layer.
%
% for i=L*L+L+1:L*L+2*L
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
% for i=3*L*L-2*L:3*L*L-L
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
%
% for i=4*L*L+L-1:L:5*L*L-1
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
%
% for i=3*L*L+2:L:4*L*L-L+2
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
% for i=5*L*L+2
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
% for i=5*L*L+L+1:5*L*L+L+2
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
% for i=7*L*L-1
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
% for i=7*L*L-L-1:7*L*L-L
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
% for i=7*L*L+L-1
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
% for i=7*L*L+2*L-1:7*L*L+2*L
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
% for i=9*L*L-L+2
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end
%
% for i=9*L*L-2*L+1:9*L*L-2*L+2
%     cellCoord=c{i};
%     for j=1:length(cellCoord)
%         inn(i,j)=cellCoord(j);
%         num(i)=length(cellCoord);
%     end
% end



%% Restructuring

for i=1:length(num)
    if num(i)~=0
        a=v(inn(i,1:num(i)),1);
        k=find(a==min(a));
        kk=floor(k(1));
        inn(i,1:num(i))=circshift(inn(i,1:num(i)),length(inn(i,1:num(i)))-kk+1);
    end

end

for i=1:length(num)
    if num(i)~=0
        a=v(inn(i,1:num(i)),1);
        if a(2)>a(num(i))
            %disp("yup")
            inn(i,1:num(i))=flip(inn(i,1:num(i)));
            inn(i,1:num(i))=circshift(inn(i,1:num(i)),length(inn(i,1:num(i)))+1);
        end
    end

end

for i=1:length(num)
    if num(i)~=0
        vx=v(inn(i,1:num(i)),1);
        vy=v(inn(i,1:num(i)),2);
        [orient,area]=polyorient(vx,vy);
        if orient
            inn(i,2:num(i))=flip(inn(i,2:num(i)));
            %inn(i,1:num(i))=circshift(inn(i,1:num(i)),length(inn(i,1:num(i)))+1);

        end
    end
end


% for i=1:length(num)
%     if num(i)~=0
%         vx=v(inn(i,1:num(i)),1);
%         vy=v(inn(i,1:num(i)),2);
%         [orient,area1]=polyorient(vx,vy);
%
%     end
% end

vInitial = v;
numInitial = num;
innInitial = inn;

% for i=1:L*L
%     vaax=vInitial(innInitial(i,1:numInitial(i)),1);
%     vaay=vInitial(innInitial(i,1:numInitial(i)),2); 
%     vaax=[vaax;vaax(1)];
%     vaay=[vaay;vaay(1)];
%     plot(vaax,vaay, color='k', LineWidth=1.5);  
%     hold on;
% end
% axis square;

end



