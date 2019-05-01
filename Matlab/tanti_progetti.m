
distanza_m = 5.0e-2;
frequenza_Hz = 3000;
angoli = -45:5:45;
margine = 15;
N = 8;
filtri = [];

for alfa = angoli
  [h, A, theta] = progetta(alfa + margine*[-1,1], N, distanza_m, frequenza_Hz);
  filtri = [filtri h];
  plot(theta, abs(A));
  hold on;
end

tmp = [angoli ; filtri];
tmp = [N; tmp(:)];
save filtri.txt tmp