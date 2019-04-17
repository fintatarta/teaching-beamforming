function [coeff, alfa, sens]=beamforming(nmic, mic_distance, direction, width, freq, speed)
% BEAMFORMING           Brief description
%
%      Synopsys:
%
%            COEFF=BEAMFORMING(NMIC, MIC_DISTANCE, DIRECTION, WIDTH, FREQ, SPEED)
%
%      Parameters:
%
%           VOID
%
%      Description:
%
%           NONE
%
%      Defaults:
%
%           SPEED = 344 m/s (sound velocity at 20C)
%
%      See also: 
%
%           NONE
%

%%
%% Default handling
%%

%
% Call parsing
%

if nargin < 6
  velocita = [];
end

%
% Default values
%

if isempty(velocita)
  velocita = 344;
end

%%
%% True code
%%

lambda = velocita / freq;  

if mic_distance > lambda/2
  warning('Mic distance %f larger than lambda/2=%f', mic_distance, lambda/2);
end

mu = mic_distance / lambda;

%
% Let D be the distance between microphones and let 
%
%    h : Z(D) -> C
%
% the "impulse response" associated with the weights of the
% mics. Let
%
%    H : R/Z(1/D) -> C 
%
% be its Fourier transform.  We know that the amplitude at 
% angle theta (theta=pi/2 if the source is directly in front to
% the array) is 
%
%    A(theta) = H((f/v) cos(theta))
%
% We want to find h in order to match the desired A.  
%
% Unfortunately, matlab works only with signals defined on Z (not Z(D)).
% We need to bring the problem to Z.  Define 
%
%   g : Z -> C
%
% as 
%
%   g(n) = h(nD)
%
% Its Fourier transform
%
%   G : R/Z -> C
%
% is related to H as
%
%   G(D f) = H(f)
%
% Therefore, it must be
%
%  A(theta) = G(D (f/v) cos(theta))
%           = G((D/lambda) cos(theta))
%
% Suppose now that we want A(theta)=a for theta \in S(a). It follows that 
% it must be
%
%     G(f) = a,   f \in (D/lambda) cos(S(a))
%
% Note that if S=[S-, S+], then cos(S) = [cos(S+), cos(S-)]
%
%
% To be more precise, I desire A(theta) as small as possible for pi/2-theta
% outside direction + [-width, width] (clipped in 0, pi/2) and near 1 
% around direction.  Therefore, it must be
%
% 0    when pi/2-theta < direction-width  <=> theta > pi/2-dir+w
% 0    when pi/2-theta > direction-width  <=> theta < pi/2-dir-w
% 1    when pi/2-theta \in d + 0.1*[-w, w] <=> theta \in pi/2-d+0.1*[]
%


%
% Convert from user reference & unit (angle measured in degrees from 
% direction orthogonal to the array) to our internal system (angle
% in radians measured from the array)
%
theta = pi/2-direction*(pi/180); 
width = width*pi/180;

if length(width) == 2
  wp = width(1);
  wa = width(2);
else
  wp = 0.1*width;
  wa = width;
end

%
% The specs of the sensitivity are
%
%    A(x) = 1       if |x-theta| < wp
%    A(x) = 0       if |x-theta| > wa
%    dont'care      otherwise
%
% with theta between 0 and pi/2.  In other words,
%
%    A(x) = 0       if 0        < x < theta-wa   stopband N. 1
%    A(x) = 1       if theta-wp < x < theta+wp   passband
%    A(x) = 0       if theta+wa < x < pi/2       stopband N. 2
%
%

pass = [max(theta-wp, 0), min(theta+wp, pi/2)];

if theta-wa > 0
  stop_1 = [0, theta-wa];
else
  stop_1 = [];
end

if theta+wa <= pi/2
  stop_2 = [theta+wa, pi/2];
else
  stop_2 = [];
end

freq = [];
ampl = [];

if (~isempty(stop_1))
  freq = stop_1;
  ampl = [0 0];
end

freq = [freq pass];
ampl = [ampl 1 1];

if (~isempty(stop_2))
  freq = [freq stop_2];
  ampl = [ampl [0 0]];
end

[freq; ampl]

freq = 2*mu*cos(freq);    %  Why 2?  Because of matlab!
[freq, perm]=sort(freq);
ampl = ampl(perm);

freq(1) = 0;
freq(end) = 1;


%if theta+w1 > pi/2
%  range = [...
%      to_freq(mu, [pi/2, theta-w1]), ...
%      to_freq(mu, [theta-width, 0])  ...
%	  ];
%  
%  value =  [1, 1, 0, 0];
%		  
%elseif theta+width > pi/2
%  range = [...
%      to_freq(mu, [theta+w1, theta-w1]), ...
%      to_freq(mu, [theta-width, 0])      ...
%	  ];
%  
%  value = [1, 1, 0, 0];
%else
%  range = [...
%      to_freq(mu, [pi/2, theta+width]),  ...
%      to_freq(mu, [theta+w1, theta-w1]), ...
%      to_freq(mu, [theta-width, 0])      ...
%	  ];
%  
%  value = [0, 0, 1, 1, 0, 0];
%end


% [freq; ampl]


coeff = remez(nmic, freq, ampl);

%
% Plot the filter frequency response
%
%[h, w]=freqz(coeff);  plot(2*w/(2*pi), abs(h))


%
% Compute the sensitivity at different angles
%
alfa_rad = 0:0.01:(pi/2);          % direction of arrival
alfa = alfa_rad * 180/pi;          % rescale to user reference

w = 2*pi*mu*cos(alfa_rad);         % freqz wants the freq. in rad
sens = abs(freqz(coeff, 1, w));


