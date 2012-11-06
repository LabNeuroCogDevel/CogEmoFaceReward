function y= myfun(x,a,b,c,d);
y1=(x.^(a-1).*(1-x).^(b-1)/beta(a,b));
y2=(x.^(c-1).*(1-x).^(d-1)/beta(c,d));

y = min(y1,y2);

%y1=betainc(x,a,b);
%y2=1-betainc(x,c,d);

%y=y1.*y2;
