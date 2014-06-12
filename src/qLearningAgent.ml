from game import *
from learningAgents import ReinforcementAgent
from featureExtractors import *

import random,util,math

class QLearningAgent(ReinforcementAgent):
    def __init__(self, **args):
        ReinforcementAgent.__init__(self, **args)
        self.qStates = util.Counter()

    def getQValue(self, state, action):
        return self.qStates[(state,action)]

    def computeValueFromQValues(self, state):
        topQval = float('-inf')
        for action in self.getLegalActions(state):
          if self.getQValue(state,action) > topQval:
            topQval = self.getQValue(state,action)
        if topQval == float('-inf'):
          return 0.0
        return topQval

    def computeActionFromQValues(self, state):
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
    
    def getValue(self, state):
        return self.computeValueFromQValues(state)

    def getPolicy(self, state):
        return self.computeActionFromQValues(state)

    def getAction(self, state):
        legalActions = self.getLegalActions(state)
        action = None
        if util.flipCoin(self.epsilon):
          return random.choice(legalActions)
        else:
          return self.computeActionFromQValues(state)
        return action

    def update(self, state, action, nextState, reward):
        topQval = float('-inf')
        self.qStates[(state,action)] = (1-self.alpha)*self.getQValue(state,action)+self.alpha*(reward+self.discount*self.computeValueFromQValues(nextState))

class PacmanQAgent(QLearningAgent):
       def __init__(self, epsilon=0.05,gamma=0.8,alpha=0.2, numTraining=0, **args):
        args['epsilon'] = epsilon
        args['gamma'] = gamma
        args['alpha'] = alpha
        args['numTraining'] = numTraining
        self.index = 0  # This is always Pacman
        QLearningAgent.__init__(self, **args)

    def getAction(self, state):
        action = QLearningAgent.getAction(self,state)
        self.doAction(state,action)
        return action

class ApproximateQAgent(PacmanQAgent):
    def __init__(self, extractor='IdentityExtractor', **args):
        self.featExtractor = util.lookup(extractor, globals())()
        PacmanQAgent.__init__(self, **args)
        self.weights = util.Counter()

    def getWeights(self):
        return self.weights

    def getQValue(self, state, action):
        feature = self.featExtractor.getFeatures(state,action)
        return self.weights*feature

    def update(self, state, action, nextState, reward):
        diff = (reward+self.discount*self.computeValueFromQValues(nextState))-self.getQValue(state,action)
        feature = self.featExtractor.getFeatures(state,action)
        for f in feature.keys():
          self.weights[f] = self.getWeights()[f]+(self.alpha*diff*feature[f])

    def final(self, state):
        PacmanQAgent.final(self, state)
        if self.episodesSoFar == self.numTraining:
            pass

