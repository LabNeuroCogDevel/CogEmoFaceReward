%% long verticle strips of scrampled faces
if ~ exist('scale','var'); scale=50; end
for facenum=1:20
   %% load face
   face  = imread(['faces/' num2str(scale) '/neutral/neutral_' num2str(facenum) '.png']);
   fcsz  = size(face,2)*size(face,1); 

   %% get random index and rearrange face
   
   % how big is a repeated region (pixels)
   % ie. number of indexes to sample from (random perm)
   blksz = 500;
   hw=fcsz/blksz;
   indx=repmat(randperm(hw),blksz,1).* blksz + repmat([1:blksz]',1,hw);
   %blksz rows of floor(hw) random samples faces index, each row's value is +1 of the above
   %  5001       14201       14501       12001   ...
   %  5002       14202       14502       12002   ...

   % some values are too big (b/c we always + blksz regardless of index value)
   indx(find(indx>hw*blksz))= 1; % set to random index that will be oversampled

   % we rounded down for dimensions, add back the rounding error
   indx = [indx(:)' 1:(  fcsz - prod(size(indx)) ) ];

   % get each color component as an el. in a cell
   facescram = {face(:,:,1),face(:,:,2),face(:,:,3) };

   % for each color comp, order in the way indx says to and reshape to the size of the image
   facescram = cellfun(@(x) reshape( x(indx), size(face,1),size(face,2) ),    facescram, 'UniformOutput',0 );
   % put color channels back in their (3rd) dim
   facescram = cat(3,facescram{:});
   % imshow(facescram)

   %% save
   outdir=['faces/' num2str(scale) '/scram'];
   if ~ exist(outdir,'dir'); mkdir(outdir); end
   imwrite(facescram,[outdir '/scram_' num2str(facenum) '.png'],'png');
end
