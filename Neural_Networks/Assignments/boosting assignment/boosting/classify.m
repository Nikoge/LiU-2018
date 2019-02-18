function cTrain = classify (data , feature , threshold , sign )
% classifies according to the best weak classifier found
cTrain = ( data ( feature ,:) > threshold ) .* sign *2 - 1* sign ;
end