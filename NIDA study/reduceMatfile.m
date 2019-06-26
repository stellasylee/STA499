files = dir('H:\\NIDA\\MatFiles');
for i = 3:(length(files)-1)
    filename = strcat('H:\\NIDA\\MatFiles\\',files(i).name);
    reduceFile(filename);
    disp(filename);
end

function reduceFile(filename)
%IMPORTFILE(FILETOREAD1)
%  Imports data from the specified file
%  filename: filepath
% Import the file
data = load('-mat', filename);
% Get 'elemDataI'
elemData = getfield (data, 'elemDataI');
% Delete unnecessary fields for analysis
unnecessary = {'AUX_CabMiscButtons','AUX_SteeringWheelButtons', 'CFS_Auto_Transmission_Mode', 'CFS_Steering_Wheel_Angle','CFS_Steering_Wheel_Angle_Rate', 'CFS_Transmission_Gear', 'CIS_Cruise_Control', 'CIS_Entertainment_Status', 'CIS_Horn','ET_approx_delay', 'ET_eye_closure_confidence', 'ET_eye_closure_frac', 'ET_eyes_closed', 'ET_filtered_gaze_object_index', 'ET_filtered_gaze_object_name' , 'ET_gaze_quality_level', 'ET_gaze_rot_unfiltered', 'ET_head_confidence', 'ET_head_pos_filtered', 'ET_head_pose_object_index', 'ET_head_pose_object_name', 'ET_head_rot_filtered', 'ET_saccade', 'SCC_Audio_Trigger', 'SCC_DynObj_CvedId', 'SCC_DynObj_DataSize', 'SCC_DynObj_HcsmType', 'SCC_DynObj_Heading', 'SCC_DynObj_Name', 'SCC_DynObj_Pos', 'SCC_DynObj_RollPitch', 'SCC_DynObj_SolId', 'SCC_DynObj_Vel', 'SCC_Lane_Depart_Warn', 'SCC_OwnVeh_Curvature', 'VDS_Chassis_CG_Accel', 'VDS_Eyepoint_Pos', 'VDS_Veh_Heading_Fixed'};

elemData = rmfield(elemData,unnecessary);
newfilename = strcat('H:\\NIDA\\Reduced\\',strrep(filename,'H:\\NIDA\\MatFiles\\',''));
% Save into new directory
save(newfilename,'elemData')
end