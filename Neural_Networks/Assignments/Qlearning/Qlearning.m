% clean up
close all
clear all

%% init the qtable to zero (all actions equally good - no randomness )
Qtable = zeros (10 , 15, 4);
% avoid that leaving the world is the best action
Qtable (1 ,: ,2) = -inf ;
Qtable (10 ,: ,1) = -inf ;
Qtable (: ,1 ,4) = -inf ;
Qtable (: ,15 ,3) = -inf ;

%% training parameters
worldToUse = 1;
episodes = 2000;
possibleAction = [1 2 3 4];
% all actions are equally likely is taken randomly
probRandomActions = [0.25 0.25 0.25 0.25];
% Long or short term rewards
gamma = 0.9;
% probility of do a random action ( epsilon in the formulas )
probRandom = 0.9;
% decrease of the probRandom per iterations ( move from exprore to exploit )
probRandomDecay = 0.999;
% Learning rate
alpha = 0.5;

%% learning process
for i =1: episodes
% update the random action probability
probRandom = probRandom * probRandomDecay ;
% initialise the world ( new random start position is some worlds )
gwinit(worldToUse)
% draw the world
gwdraw
% get the state of the robot
state = gwstate ();
while state.isterminal ~= 1
% save old position
oldPosition = state.pos;
% choose an action based on the Qtable ( optimal ) or random . Adjust
% how often random actions are taken using the probRandom parameter
action = chooseaction(Qtable,oldPosition(1),oldPosition(2),possibleAction,eps,probRandomActions,[1 -probRandom,probRandom]);
% take an action and get the resulting state of the robot
state = gwaction(action);
% update the Qtable only if the action was valid (don 't leave the world )
if (state.isvalid)
Qtable(oldPosition(1),oldPosition(2),action) = (1 - alpha)*Qtable(oldPosition(1), oldPosition(2), action) + alpha*(state.feedback + gamma*max(Qtable(state.pos(1),state.pos(2), :))); % the change to it
gwplotarrow(oldPosition, action);
end
end
end

%% plot the V function (the best actions depending on the state)
% draw the world without any arrows
gwdraw
% extract the optimal directions (optimal policy)
[~, I] = max (Qtable, [], 3)
% draw the arrows for each grid point
for xx = 1:10
for yy = 1:15
gwplotarrow ([xx; yy], I(xx, yy));
end
end