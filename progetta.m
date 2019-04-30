function [h, A, theta]=progetta (angles, N, step, freq, vel) 
  
  if nargin < 5 
    freq = 3000;
  end
  
  if nargin < 6
    vel = 335; 
  end
  
  K = step*freq/vel;
  
  %
  % FROM and TO are given by the user as angles with respect
  % to the  direction orthogonal to the axis where the microphones
  % are placed, see the picture
  %
  %                 |      /
  %                 |     /
  %                 | U  /
  %                 |   /
  %                 |  /
  %                 | / Theta
  %                 |/
  %  ----*--*--*----+--*--*--*--*-------
  %
  % The '*' are the microphones, U is the angle given by the 
  % user and Theta is the angle we need.  It is clear that
  % Theta = pi/2-U  (U in figure is positive)
  %
  
  angles = angles*pi/180;
  
  from = pi/2 - angles(1);
  to   = pi/2 - angles(2);
  
  f_lo = K*cos(from)
  f_hi = K*cos(to)
  middle = K*cos((from+to)/2);
  
  %
  % Now, I want a filter whose frequency response in middle is 1
  % and it has a frequency response as small as possible outside
  % the frequency interval [f_lo, f_hi].  The condition of being
  % equal to 1 in middle can be expressed as%
  %
  %     W' * h = 1
  %
  % where h is the vector of filter coefficients and 
  %
  %  W(k) = exp(-j*2*pi*middle*k), k=0, 1, ...
  %
  % It turns out that h can be written as [1, 0, ..., 0]+u where
  % u belongs to the null space of W'
  %
  %global base_null
  
  n = (0:(N-1));
  
  W = exp(j*2*pi*middle*n);
  
  f = -0.5:0.001:0.5;
  idx = find((f > f_lo) .* (f < f_hi));
  f(idx) = [];
  
  
  sampler = exp(j*2*pi*f'*n);
  base_null=null(W);
  
  ddd=norm(W*base_null)
  
  h=fminunc(@(x) zilopano(x, base_null, sampler), rand(1, 2*(N-1)));
  h=[1; zeros(N-1,1)] + base_null*complessify(h);
  
  dvrei=W*h
  
  theta = -pi/2:0.001:pi/2;
  f = K*cos(pi/2-theta);
  theta = theta*180/pi;
  A = exp(j*2*pi*f'*n)*h;
  
  end
  function x=zilopano(u, base, s)
    
    u=complessify(u);
    u0=[1; zeros(size(u))];
    x=max(abs(s*(u0 + base*u)));
  end
  
  function v=complessify(u)
    v = reshape(u, length(u)/2, 2);
    v =  v * [1; j]; 
   end