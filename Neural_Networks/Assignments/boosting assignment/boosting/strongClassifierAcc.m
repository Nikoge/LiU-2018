function accuracy = strongClassifierAcc (data , labels , feature , threshold ,sign , alpha )
decVal = zeros (1, size (data ,2) );
numbersOfClassifiers = length ( feature );
for jj = 1: numbersOfClassifiers
decVal = decVal + alpha (jj) .* classify (data , feature (jj), threshold (jj), sign (jj));
end

% signum function
decVal ( decVal >0) = 1;
 decVal ( decVal <=0) = -1;

 %% calc accuracy
 accuracy = sum (( decVal .* labels ) > 0) / length ( labels );
end
