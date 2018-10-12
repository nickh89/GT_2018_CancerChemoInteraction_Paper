function varargout = errorellipse(data)
%returns an ellipse showing standard deviation
%of data suitable for plot(X,Y) or line(X,Y)
%[X, Y, Z] = errorellipse(data) returns an ellipse showing standard deviation
%of data suitable for surf(X,Y,Z)

t=0:pi/20:2*pi;

%Screen for NaNs
bad = sum(isnan(data),2);
data = data(bad==0,:);

origin = mean(data);
covmat = cov(data);
[v d] = eig(covmat);
d = sqrt(diag(d));

if size(data,2)==2
    ellipse = v*[d(1)*cos(t);d(2)*sin(t)];
    varargout{1} = ellipse(1,:)+origin(1);
    varargout{2} = ellipse(2,:)+origin(2);
else
    spherepoints = 21;
    [x y z] = sphere(spherepoints-1);
    x = x*d(1);
    y = y*d(2);
    z = z*d(3);
    ellipse = v*[x(:)';y(:)';z(:)'];
    varargout{1} = reshape(ellipse(1,:),spherepoints, spherepoints)+origin(1);
    varargout{2} = reshape(ellipse(2,:),spherepoints, spherepoints)+origin(2);
    varargout{3} = reshape(ellipse(3,:),spherepoints, spherepoints)+origin(3);
end