from game import *
from learningAgents import ReinforcementAgent
from featureExtractors import *
import random,util,math

module type TemplateParameters =
sig
    type ReinforcementAgent
end;;

module  QLearningAgent = functor (T : TemplateParameters) ->
struct
    let __init__(self, **args):
        ReinforcementAgent.__init__(self, **args)
        self.qStates = util.Counter()

    let getQValue(self, state, action):
        return self.qStates[(state,action)]

    let computeValueFromQValues(self, state):
        topQval = float('-inf')
        for action in self.getLegalActions(state):
          if self.getQValue(state,action) > topQval:
            topQval = self.getQValue(state,action)
        if topQval == float('-inf'):
          return 0.0
        return topQval

    let computeActionFromQValues(self, state):
        bestActions = []
        bestValue = float('-inf')
        for action in self.getLegalActions(state):
          if self.getQValue(state,action) > bestValue:
            bestValue = self.getQValue(state,action)
            bestActions = [action]
          elif self.getQValue(state,action) == bestValue:
            bestActions.append(action)
        if len(bestActions) != 0:
          return random.choice(bestActions) 
        return None
    
    let getValue(self, state):
        return self.computeValueFromQValues(state)

    let getPolicy(self, state):
        return self.computeActionFromQValues(state)

    let getAction(self, state):
        legalActions = self.getLegalActions(state)
        action = None
        if util.flipCoin(self.epsilon):
          return random.choice(legalActions)
        else:
          return self.computeActionFromQValues(state)
        return action

    let update(self, state, action, nextState, reward):
        topQval = float('-inf')
        self.qStates[(state,action)] = (1-self.alpha)*self.getQValue(state,action)+self.alpha*(reward+self.discount*self.computeValueFromQValues(nextState))
end;;

module type TemplateParameters =
sig
    type QLearningAgent
end;;

module PacmanQAgent = functor (QLearningAgent) ->
struct
    let __init__(self, epsilon=0.05,gamma=0.8,alpha=0.2, numTraining=0, **args):
        args['epsilon'] = epsilon
        args['gamma'] = gamma
        args['alpha'] = alpha
        args['numTraining'] = numTraining
        self.index = 0  # This is always Pacman
        QLearningAgent.__init__(self, **args)

    let getAction(self, state):
        action = QLearningAgent.getAction(self,state)
        self.doAction(state,action)
        return action

module type TemplateParameters =
sig
    type PacmanAgent
end;;

module ApproximateQAgent = functor (T : TemplateParameters) ->
struct
    let __init__(self, extractor='IdentityExtractor', **args):
        self.featExtractor = util.lookup(extractor, globals())()
        PacmanQAgent.__init__(self, **args)
        self.weights = util.Counter()

    let getWeights(self):
        return self.weights

    let getQValue(self, state, action):
        feature = self.featExtractor.getFeatures(state,action)
        return self.weights*feature

    let update(self, state, action, nextState, reward):
        diff = (reward+self.discount*self.computeValueFromQValues(nextState))-self.getQValue(state,action)
        feature = self.featExtractor.getFeatures(state,action)
        for f in feature.keys():
          self.weights[f] = self.getWeights()[f]+(self.alpha*diff*feature[f])

    let final(self, state):
        PacmanQAgent.final(self, state)
        if self.episodesSoFar == self.numTraining:
            pass

end;;
