function y= myfun(x,a,b);
y1=(x.^(a-1).*(1-x).^(b-1)/beta(a,b));
y2=log(1./y1);
y=y1.*y2;