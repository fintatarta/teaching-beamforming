function [h, A, theta]=design_beamformer (angles, N, step, freq, vel) 
% DESIGN_BEAMFORMER           Brief description
%
%      Synopsys:
%
%            [H, A, THETA]=DESIGN_BEAMFORMER (ANGLES, N, STEP, FREQ, VEL) 
%
%      Parameters:
%
%           ANGLES = [FROM TO]
%           N      = Number of microphone
%           STEP   = Distance between mics in meters
%           FREQ   = Frequency of the signal in Hz
%           VEL    = Speed of sound in m/s
%
%      Description:
%
%           NONE
%
%      Defaults:
%
%           FREQ = 3000 Hz
%           VEL  = 335 m/s
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

%
% Default values
%

%%
%% True code
%%

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
% user (in degrees) and Theta is the angle we need.  It is clear that
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
%     W * h = 1
%
% where h is the vector of filter coefficients and W is the row vector
%
%  W(k) = exp(-j*2*pi*middle*k), k=0, 1, ...
%
% It turns out that h can be written as [1, 0, ..., 0]+u where
% u belongs to the null space of W.  Therefore, the problem can 
% be made unconstrained by searching for the optimal X \in R^{N-1}
% such that
%
%    h = u0 + B*X
%
% minimizes the maximum value of the Fourier transform outside the
% "bandpass".  In the equation above u0 = [1, 0, ..., 0] and B
% is a basis of the null space of W'
%      
%


n = (0:(N-1));

u0 = [1; zeros(N-1, 1)];
W = exp(j*2*pi*middle*n);
base_null=null(W);

%
% Sanity check
%
if norm(W*base_null) > 1.0e-4 
  error('BUG: W*base_null too large');
end

%
% We do a brutal optimization by sampling the response 
% over a number of frequencies outside the "passband" from .. to.
%
f = -0.5:0.001:0.5;                    % Frequency set
idx = find((f > f_lo) .* (f < f_hi));  % Remove the freq in the passband
f(idx) = [];

%
% By multiplying sampler by the "impulse response" we get the 
% frequency response at the frequencies in f
%
sampler = exp(j*2*pi*f'*n);


%
% Find the component with respect to base_null of the optimal
% vector orthogonal to W 
%
X=fminunc(@(x) objective(x, u0, base_null, sampler), rand(N-1, 2));

%
% Apply the basis and add the bias to satisfy the constraint of 
% unitary response at MIDDLE
%
h=u0 + base_null*complessify(X);


%
% Compute the response
%
theta = -pi/2:0.001:pi/2;
f = K*cos(pi/2-theta);
A = exp(j*2*pi*f'*n)*h;

%
% Give to the user theta in degrees
%
theta = theta*180/pi;

function x=objective(u, u0, base, sampler)

u=complessify(u);
h = u0 + base*u;

x=max(abs(sampler*h));

function v=complessify(u)

v =  u * [1 ; j];
