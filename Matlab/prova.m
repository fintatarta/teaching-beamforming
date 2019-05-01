n = (0:7)';
f = 0.2;
W = exp(j*2*pi*f*n);

U = (W*W').*(n-n');

if 0 
  % sanity check 
  W2 = exp(j*2*pi*(f+0.00005)*n);
  Q = 20000*((W2*W2')-(W*W'));
  angle(Q./(j*2*pi*U)) / pi
end


[A, D]=eig(U);
% here U = A*D*A'
% D is mostly zero
idx = find(abs(diag(D)) > 0.1);
J = eye(length(n));
J(:,idx)=[];

B=A'*J