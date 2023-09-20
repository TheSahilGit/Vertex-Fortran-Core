close all; clear; clc; 

vInitial = load("v_in.dat");
innInitial = load("inn_in.dat");
numInitial = load("num_in.dat");

vOut = load('vOut.dat');
innOut = load("innOut.dat");
numOut = load("numOut.dat");
En = load("Energy.dat");
para2 = load("para2_in.dat");
para1 = readtable("para1_in.dat");
etas = load("motility_in.dat");

L = para2(1);
dt = table2array(para1(8,1));
totTime = table2array(para1(7,1))*(dt);

time  = linspace(0,totTime,length(En));
mainarea=(1:L*L);
leftpanel=(1:L);
rightpanel=(L*L-L+1:L*L);
toppanel=(L:L:L*L);
bottompanel=(1:L:L*L-L+1);
corners=[1 L L*L-L+1 L*L ];

Boundary = [leftpanel rightpanel toppanel bottompanel];




%%--
Plot_Tissue(L,vInitial,innInitial,numInitial,vOut, innOut,...
    numOut, Boundary, totTime, etas)

Plot_Energy(En, time)

%Plot_EdgeLenghDist(L, numInitial, numOut)

%%--



%% Energy time plot

function [] = Plot_Energy(En, time)
figure()
plot(time, En, LineWidth=3, Marker=".")
axis square
xlabel("Time")
ylabel("Energy")
set(gca, fontsize=30, fontname= "Times", fontweight="Bold")

end

%% Edgelength distribution plot

function [] = Plot_EdgeLenghDist(L, numInitial, numOut)

[values, edges] = histcounts(numInitial(1:L*L), 'Normalization', 'probability');
centers = (edges(1:end-1)+edges(2:end))/2;
figure
plot(centers, values, DisplayName="Initial", LineWidth=2.5)

hold on;


[values, edges] = histcounts(numOut(1:L*L), 'Normalization', 'probability');
centers = (edges(1:end-1)+edges(2:end))/2;
plot(centers, values, DisplayName="Final", LineWidth=2.5)

legend()

axis square
xlabel("Vertex No")
ylabel("Count")
set(gca, fontsize=18, fontname= "Times", fontweight="Bold")
end

%% Tissue plot

function []= Plot_Tissue(L,vInitial,innInitial,numInitial,vOut, innOut,...
    numOut, Boundary, totTime, etas)


figure();

%%--

subplot(1,2,1)
for i=1:L*L %length(num)
    vx=vInitial(innInitial(i,1:numInitial(i)),1);
    vy=vInitial(innInitial(i,1:numInitial(i)),2);
    pl = polyshape(vx,vy);
    plot(pl, FaceColor=[0.6350 0.0780 0.1840], FaceAlpha=0.1, LineWidth=1.5)
    hold on;
end

%title("t=",num2str(totTime))
axis square
for ii=1:length(Boundary) %length(num)
    i = Boundary(ii);
    vx = vInitial(innInitial(i,1:numInitial(i)),1);
    vy = vOut(innInitial(i,1:numInitial(i)),2);
    pl  = polyshape(vx,vy);
    plot(pl, FaceColor='k', FaceAlpha=0.9, LineWidth=1.5)
    hold on;
end

%colormap turbo
%hCB = colorbar();

axis square;

title("t=0");


subplot(1,2,2)




for i=1:L*L %length(num)
    vx = vOut(innOut(i,1:numOut(i)),1);
    vy = vOut(innOut(i,1:numOut(i)),2);
    pl = polyshape(vx,vy);
    plot(pl, FaceColor='g', FaceAlpha=0.3, LineWidth=1.5)
    hold on;
end

%colormap turbo

% hcb = colorbar();
% clim([min(etas) max(etas)])
% hcb.Title.String = "\eta_s";
% 




%%scatter(cellcent(:,1), cellcent(:,2), "filled")

% etas = etas/max(etas);
% motgrad = 1;
% 
% workingzone = (1:L*L);
% insidebulk = setdiff(workingzone,Boundary);
% 
% if motgrad==1
%     for ii=1:length(insidebulk)
%         i = insidebulk(ii);
%         vx = vOut(innOut(i,1:numOut(i)),1);
%         vy = vOut(innOut(i,1:numOut(i)),2);
%         vx=[vx;vx(1)];
%         vy=[vy;vy(1)];
%         p = etas(innOut(i,1:numOut(i)));
%         for j=1,length(vx)
%             scatter(vx(j),vy(j),20, 'MarkerEdgeColor',[p(j) 0 0],'MarkerFaceColor',...
%             [p(j) 0 0] );
%         end
%         hold on;
%     end
% end


for ii=1:length(Boundary) %length(num)
    i = Boundary(ii);
    vx=vOut(innOut(i,1:numOut(i)),1);
    vy=vOut(innOut(i,1:numOut(i)),2);
    pl  = polyshape(vx,vy);
    plot(pl, FaceColor='k', FaceAlpha=1, LineWidth=1.5)
    hold on;
end
axis square

title(strcat(["t=",num2str(totTime)]))

%--
end






