function phaseScrambleImage(infile, outfile)
%from here: http://visionscience.com/pipermail/visionlist/2007/002181.html
%similar to this article: http://www.frontiersin.org/human_neuroscience/10.3389/neuro.09.067.2009/full
%adapted to obtain and respect alpha channel from png images (to make ovals)

[Im colormap alpha] = imread(infile);

Im = mat2gray(double(Im));

%read and rescale (0-1) image

ImSize = size(Im);

RandomPhase = angle(fft2(rand(ImSize(1), ImSize(2))));
%generate random phase structure

for layer = 1:ImSize(3)
    %Fast-Fourier transform
    ImFourier(:,:,layer) = fft2(Im(:,:,layer));       
    
    %amplitude spectrum
    Amp(:,:,layer) = abs(ImFourier(:,:,layer));       

    %phase spectrum
    Phase(:,:,layer) = angle(ImFourier(:,:,layer));   

    %add random phase to original phase
    Phase(:,:,layer) = Phase(:,:,layer) + RandomPhase;

    %combine Amp and Phase then perform inverse Fourier
    ImScrambled(:,:,layer) = ifft2(Amp(:,:,layer).*exp(sqrt(-1)*(Phase(:,:,layer))));   

end

ImScrambled = real(ImScrambled); %get rid of imaginery part in image (due to rounding error)

%psychtoolbox doesn't respect transparency by default.
%mat2gray above appears to convert 255 to 0 (black)
%so, set the periphery of ellipse to white manually
[maskx masky] = find(alpha == 0);
for i = 1:length(maskx)
    ImScrambled(maskx(i), masky(i), :) = 1; %white
end

imwrite(ImScrambled,outfile,'png', 'Alpha', alpha);

imshow(ImScrambled)