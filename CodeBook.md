# Codebook for 'tidy_mean.txt'


## Tranformations

The following steps have been performed to attain a tidy dataset. 

- Read data.
- Merge the training and the test sets to create one data set.
- Extract the mean and standard deviation for each measurement.
- Use descriptive activity names to name the activities in the data set.
- Appropriately label the data set with descriptive variable names.
- Create a second, independent tidy set with the average of each variable for each activity and each subject.
- Write the data set to the 'tidy_mean.txt' file.


## Variables

SubjectId                                                          : int  1 1 1 1 1 1 2 2 2 2 ...

SubjectId: ID of the 30 volunteers who performed the activity. Its range is from 1 to 30.


Activity                                                           : Factor w/ 6 levels "WALKING","WALKING_UPSTAIRS",..: 1 2 3 4 5 6 1 2 3 4 ...

Activity: Each person performed six activities wearing a smartphone (Samsung Galaxy S II) on the waist. Factor variable with 6 levels:
1. WALKING
2. WALKING_UPSTAIRS
3. WALKING_DOWNSTAIRS
4. SITTING
5. STANDING
6. LAYING


## Features and values

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals timeAccelerometer a-XYZ and timeGyroscope-XYZ. These time domain signals were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (timeBodyacceleration-XYZ and timeGravityAcceleration-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz.

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (timeBodyAccelerationJerk-XYZ and timeBodyGyroscopeJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (timeBodyAccelerationMagnitude, timeGravityAccelerationMagnitude, timeBodyAccelerationJerkMagnitude, timeBodyGyroscopeMagnitude, timeBodyGyroscopeJerkMagnitude).

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcceleration-XYZ, frequencyBodyAccelerationJerk-XYZ, frequencyBodyGyro-XYZ, frequencyBodyAccelerationJerkMagnitude, frequencyBodyGyroMagnitude, frequencyBodyGyroJerkMagnitude.

These signals were used to estimate variables of the feature vector for each pattern:
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

Values are in the interval [-1,1]

  
  TimeDomainBodyAccelerometerMeanValueX                              : num  0.277 0.255 0.289 0.261 0.279 ...
  TimeDomainBodyAccelerometerMeanValueY                              : num  -0.01738 -0.02395 -0.00992 -0.00131 -0.01614 ...
  TimeDomainBodyAccelerometerMeanValueZ                              : num  -0.1111 -0.0973 -0.1076 -0.1045 -0.1106 ...
  TimeDomainBodyAccelerometerStandardDeviationX                      : num  -0.284 -0.355 0.03 -0.977 -0.996 ...
  TimeDomainBodyAccelerometerStandardDeviationY                      : num  0.11446 -0.00232 -0.03194 -0.92262 -0.97319 ...
  TimeDomainBodyAccelerometerStandardDeviationZ                      : num  -0.26 -0.0195 -0.2304 -0.9396 -0.9798 ...
  TimeDomainGravityAccelerometerMeanValueX                           : num  0.935 0.893 0.932 0.832 0.943 ...
  TimeDomainGravityAccelerometerMeanValueY                           : num  -0.282 -0.362 -0.267 0.204 -0.273 ...
  TimeDomainGravityAccelerometerMeanValueZ                           : num  -0.0681 -0.0754 -0.0621 0.332 0.0135 ...
  TimeDomainGravityAccelerometerStandardDeviationX                   : num  -0.977 -0.956 -0.951 -0.968 -0.994 ...
  TimeDomainGravityAccelerometerStandardDeviationY                   : num  -0.971 -0.953 -0.937 -0.936 -0.981 ...
  TimeDomainGravityAccelerometerStandardDeviationZ                   : num  -0.948 -0.912 -0.896 -0.949 -0.976 ...
  TimeDomainBodyAccelerometerJerkMeanValueX                          : num  0.074 0.1014 0.0542 0.0775 0.0754 ...
  TimeDomainBodyAccelerometerJerkMeanValueY                          : num  0.028272 0.019486 0.02965 -0.000619 0.007976 ...
  TimeDomainBodyAccelerometerJerkMeanValueZ                          : num  -0.00417 -0.04556 -0.01097 -0.00337 -0.00369 ...
  TimeDomainBodyAccelerometerJerkStandardDeviationX                  : num  -0.1136 -0.4468 -0.0123 -0.9864 -0.9946 ...
  TimeDomainBodyAccelerometerJerkStandardDeviationY                  : num  0.067 -0.378 -0.102 -0.981 -0.986 ...
  TimeDomainBodyAccelerometerJerkStandardDeviationZ                  : num  -0.503 -0.707 -0.346 -0.988 -0.992 ...
  TimeDomainBodyGyroscopeMeanValueX                                  : num  -0.0418 0.0505 -0.0351 -0.0454 -0.024 ...
  TimeDomainBodyGyroscopeMeanValueY                                  : num  -0.0695 -0.1662 -0.0909 -0.0919 -0.0594 ...
  TimeDomainBodyGyroscopeMeanValueZ                                  : num  0.0849 0.0584 0.0901 0.0629 0.0748 ...
  TimeDomainBodyGyroscopeStandardDeviationX                          : num  -0.474 -0.545 -0.458 -0.977 -0.987 ...
  TimeDomainBodyGyroscopeStandardDeviationY                          : num  -0.05461 0.00411 -0.12635 -0.96647 -0.98773 ...
  TimeDomainBodyGyroscopeStandardDeviationZ                          : num  -0.344 -0.507 -0.125 -0.941 -0.981 ...
  TimeDomainBodyGyroscopeJerkMeanValueX                              : num  -0.09 -0.1222 -0.074 -0.0937 -0.0996 ...
  TimeDomainBodyGyroscopeJerkMeanValueY                              : num  -0.0398 -0.0421 -0.044 -0.0402 -0.0441 ...
  TimeDomainBodyGyroscopeJerkMeanValueZ                              : num  -0.0461 -0.0407 -0.027 -0.0467 -0.049 ...
  TimeDomainBodyGyroscopeJerkStandardDeviationX                      : num  -0.207 -0.615 -0.487 -0.992 -0.993 ...
  TimeDomainBodyGyroscopeJerkStandardDeviationY                      : num  -0.304 -0.602 -0.239 -0.99 -0.995 ...
  TimeDomainBodyGyroscopeJerkStandardDeviationZ                      : num  -0.404 -0.606 -0.269 -0.988 -0.992 ...
  TimeDomainBodyAccelerometerMagnitudeMeanValue                      : num  -0.137 -0.1299 0.0272 -0.9485 -0.9843 ...
  TimeDomainBodyAccelerometerMagnitudeStandardDeviation              : num  -0.2197 -0.325 0.0199 -0.9271 -0.9819 ...
  TimeDomainGravityAccelerometerMagnitudeMeanValue                   : num  -0.137 -0.1299 0.0272 -0.9485 -0.9843 ...
  TimeDomainGravityAccelerometerMagnitudeStandardDeviation           : num  -0.2197 -0.325 0.0199 -0.9271 -0.9819 ...
  TimeDomainBodyAccelerometerJerkMagnitudeMeanValue                  : num  -0.1414 -0.4665 -0.0894 -0.9874 -0.9924 ...
  TimeDomainBodyAccelerometerJerkMagnitudeStandardDeviation          : num  -0.0745 -0.479 -0.0258 -0.9841 -0.9931 ...
  TimeDomainBodyGyroscopeMagnitudeMeanValue                          : num  -0.161 -0.1267 -0.0757 -0.9309 -0.9765 ...
  TimeDomainBodyGyroscopeMagnitudeStandardDeviation                  : num  -0.187 -0.149 -0.226 -0.935 -0.979 ...
  TimeDomainBodyGyroscopeJerkMagnitudeMeanValue                      : num  -0.299 -0.595 -0.295 -0.992 -0.995 ...
  TimeDomainBodyGyroscopeJerkMagnitudeStandardDeviation              : num  -0.325 -0.649 -0.307 -0.988 -0.995 ...
  FrequencyDomainBodyAccelerometerMeanValueX                         : num  -0.2028 -0.4043 0.0382 -0.9796 -0.9952 ...
  FrequencyDomainBodyAccelerometerMeanValueY                         : num  0.08971 -0.19098 0.00155 -0.94408 -0.97707 ...
  FrequencyDomainBodyAccelerometerMeanValueZ                         : num  -0.332 -0.433 -0.226 -0.959 -0.985 ...
  FrequencyDomainBodyAccelerometerStandardDeviationX                 : num  -0.3191 -0.3374 0.0243 -0.9764 -0.996 ...
  FrequencyDomainBodyAccelerometerStandardDeviationY                 : num  0.056 0.0218 -0.113 -0.9173 -0.9723 ...
  FrequencyDomainBodyAccelerometerStandardDeviationZ                 : num  -0.28 0.086 -0.298 -0.934 -0.978 ...
  FrequencyDomainBodyAccelerometerMeanValueFrequencyX                : num  -0.2075 -0.4187 -0.3074 -0.0495 0.0865 ...
  FrequencyDomainBodyAccelerometerMeanValueFrequencyY                : num  0.1131 -0.1607 0.0632 0.0759 0.1175 ...
  FrequencyDomainBodyAccelerometerMeanValueFrequencyZ                : num  0.0497 -0.5201 0.2943 0.2388 0.2449 ...
  FrequencyDomainBodyAccelerometerJerkMeanValueX                     : num  -0.1705 -0.4799 -0.0277 -0.9866 -0.9946 ...
  FrequencyDomainBodyAccelerometerJerkMeanValueY                     : num  -0.0352 -0.4134 -0.1287 -0.9816 -0.9854 ...
  FrequencyDomainBodyAccelerometerJerkMeanValueZ                     : num  -0.469 -0.685 -0.288 -0.986 -0.991 ...
  FrequencyDomainBodyAccelerometerJerkStandardDeviationX             : num  -0.1336 -0.4619 -0.0863 -0.9875 -0.9951 ...
  FrequencyDomainBodyAccelerometerJerkStandardDeviationY             : num  0.107 -0.382 -0.135 -0.983 -0.987 ...
  FrequencyDomainBodyAccelerometerJerkStandardDeviationZ             : num  -0.535 -0.726 -0.402 -0.988 -0.992 ...
  FrequencyDomainBodyAccelerometerJerkMeanValueFrequencyX            : num  -0.209 -0.377 -0.253 0.257 0.314 ...
  FrequencyDomainBodyAccelerometerJerkMeanValueFrequencyY            : num  -0.3862 -0.5095 -0.3376 0.0475 0.0392 ...
  FrequencyDomainBodyAccelerometerJerkMeanValueFrequencyZ            : num  -0.18553 -0.5511 0.00937 0.09239 0.13858 ...
  FrequencyDomainBodyGyroscopeMeanValueX                             : num  -0.339 -0.493 -0.352 -0.976 -0.986 ...
  FrequencyDomainBodyGyroscopeMeanValueY                             : num  -0.1031 -0.3195 -0.0557 -0.9758 -0.989 ...
  FrequencyDomainBodyGyroscopeMeanValueZ                             : num  -0.2559 -0.4536 -0.0319 -0.9513 -0.9808 ...
  FrequencyDomainBodyGyroscopeStandardDeviationX                     : num  -0.517 -0.566 -0.495 -0.978 -0.987 ...
  FrequencyDomainBodyGyroscopeStandardDeviationY                     : num  -0.0335 0.1515 -0.1814 -0.9623 -0.9871 ...
  FrequencyDomainBodyGyroscopeStandardDeviationZ                     : num  -0.437 -0.572 -0.238 -0.944 -0.982 ...
  FrequencyDomainBodyGyroscopeMeanValueFrequencyX                    : num  0.0148 -0.1875 -0.1005 0.1892 -0.1203 ...
  FrequencyDomainBodyGyroscopeMeanValueFrequencyY                    : num  -0.0658 -0.4736 0.0826 0.0631 -0.0447 ...
  FrequencyDomainBodyGyroscopeMeanValueFrequencyZ                    : num  0.000773 -0.133374 -0.075676 -0.029784 0.100608 ...
  FrequencyDomainBodyAccelerometerMagnitudeMeanValue                 : num  -0.1286 -0.3524 0.0966 -0.9478 -0.9854 ...
  FrequencyDomainBodyAccelerometerMagnitudeStandardDeviation         : num  -0.398 -0.416 -0.187 -0.928 -0.982 ...
  FrequencyDomainBodyAccelerometerMagnitudeMeanValueFrequency        : num  0.1906 -0.0977 0.1192 0.2367 0.2846 ...
  FrequencyDomainBodyBodyAccelerometerJerkMagnitudeMeanValue         : num  -0.0571 -0.4427 0.0262 -0.9853 -0.9925 ...
  FrequencyDomainBodyBodyAccelerometerJerkMagnitudeStandardDeviation : num  -0.103 -0.533 -0.104 -0.982 -0.993 ...
  FrequencyDomainBodyBodyAccelerometerJerkMagnitudeMeanValueFrequency: num  0.0938 0.0854 0.0765 0.3519 0.4222 ...
  FrequencyDomainBodyBodyGyroscopeMagnitudeMeanValue                 : num  -0.199 -0.326 -0.186 -0.958 -0.985 ...
  FrequencyDomainBodyBodyGyroscopeMagnitudeStandardDeviation         : num  -0.321 -0.183 -0.398 -0.932 -0.978 ...
  FrequencyDomainBodyBodyGyroscopeMagnitudeMeanValueFrequency        : num  0.268844 -0.219303 0.349614 -0.000262 -0.028606 ...
  FrequencyDomainBodyBodyGyroscopeJerkMagnitudeMeanValue             : num  -0.319 -0.635 -0.282 -0.99 -0.995 ...
  FrequencyDomainBodyBodyGyroscopeJerkMagnitudeStandardDeviation     : num  -0.382 -0.694 -0.392 -0.987 -0.995 ...
  FrequencyDomainBodyBodyGyroscopeJerkMagnitudeMeanValueFrequency    : num  0.191 0.114 0.19 0.185 0.334 ...

