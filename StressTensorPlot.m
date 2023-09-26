clear; clc; close all; 

data = load("StressTensor.dat");
para1 = readtable("para1_in.dat");
para2 = load("para2_in.dat");



pressure = zeros(1,length(data(:,1))/2);
shear = pressure;
c=1;
for t=1:2:length(data(:,1))
    sigma(1,:) = data(t,:) ;
    sigma(2,:) = data(t+1,:);
    pressure(c) = trace(sigma);
    shear(c) = sigma(1,2);
    c=c+1;
end


fftshear = fft(shear);


L = para2(1);
dt = table2array(para1(8,1));
totTime = table2array(para1(7,1))*(dt);
beta = para1.Var1(4);
gamma = para1.Var1(5);


time  = (0:dt:totTime-dt);

%figure()
subplot(1,2,1)
hold on;
plot(time,pressure, LineWidth=3, DisplayName=strcat("\beta , \gamma = " , ...
    num2str(beta),",",num2str(gamma)))
axis square
legend()
axis square
xlabel("Time")
ylabel("Pressure")
set(gca, fontsize=30, fontname= "Times", fontweight="Bold")

%figure()
subplot(1,2,2)
hold on;
plot(time, shear, LineWidth=3, DisplayName=strcat("\beta , \gamma = " , ...
    num2str(beta),",",num2str(gamma)))
axis square
legend()

axis square
xlabel("Time")
ylabel("Shear Stress")
set(gca, fontsize=30, fontname= "Times", fontweight="Bold")




% figure()
% plot(imag(fftshear))