module HushGP.GP.PushArgs where

import HushGP.State
import HushGP.Instructions
import HushGP.GP.PushData

-- | The structure holding the arguments for the various aspects
-- of the evolutionary run in Hush.
data PushArgs = PushArgs
  {
    -- | For alternation, std deviation for index when alternating.
    alignmentDeviation :: Double,
    -- | For alternation, probability of switching parents at each location. Should be a value in the range [1,100]
    alternationRate :: Double,
    -- | For bmx, rate genes are exchanged.
    bmxExchangeRate :: Float,
    -- | For bmx, max length of a gene.
    bmxGeneLengthLimit :: Int,
    -- | For bmx, mutation rate for gaps.
    bmxGapChangeProbability :: Float,
    -- | For bmx, whether mates selected using reverse case sequences of first parent
    bmxIsComplementary :: Bool,
    -- | For bmx, don't exchange distance if greater than this
    bmxMaxDistance :: Int,
    -- | For bmx, only allow exchanges between individual with same number of genes.
    bmxSameGeneCount :: Bool,
    -- | For bmx, swap segment with same sequence index, not by best match
    ssxNotBmx :: Bool,
    -- | Ways to construct a phenotype from a plushy genome, so far only "specified" is implemented. Unused (for now).
    closes :: String,
    -- | Whether or not to use best match crossover
    useBMX :: Bool,
    -- | Custom report for each generation if provided.
    customReport :: Maybe (PushArgs -> IO ()),
    -- | If True, keeps running regardless of success.
    dontEnd :: Bool,
    -- | Whether of not to use downsampling.
    enableDownsampling :: Bool,
    -- | The downsample function to use. "caseRand", "caseMaxim", "caseMaximAuto".
    downsampleFunction :: String,
    -- | Proportion of data used in downsample.
    downsampleRate :: Float,
    -- | Proportion of parents used to evaluate case distances.
    downsampleParentRate :: Float,
    -- | Amount of generations between parent distance computation
    downsampleParentsGens :: Int,
    -- | Whether or not to add the best individual to the next generation.
    elitism :: Bool,
    -- | User must provide their own error function.
    -- Arg 1: PushArgs for the current set of arguments.
    -- Arg 2: [PushData] is the input data.
    -- Arg 3: [Gene] is the plushy representation of a program.
    -- Returns the error list for a given set of inputs of type [Double].
    errorFunction :: PushArgs -> [PushData] -> [Gene] -> [Double],
    -- | Type of informed downsampling. "solved", "elite", "soft".
    informedDownsamplingType :: String,
    -- | List of instructions to use in the evolutionary run.
    instructionList :: [Gene],
    -- | For motely batch lexicase selection, max size of a batch of cases.
    maxMotelyBatchSize :: Int,
    -- | Max size of plushy genomes in a population.
    maxInitialPlushySize :: Int,
    -- | Maximum amount of generations allowed in an evolutionary run.
    maxGenerations :: Int,
    -- | Type of parent selection to use. Options are: "tournament","lexicase","epsilonLexicase".
    parentSelectionAlgo :: String,
    -- |Size of the population in the evolutionary run.
    populationSize :: Int,
    -- | For uniform replacement, rate of item replacement. A number in the bounds of [1,100].
    replacementRate :: Double,
    -- | Whether or not to auto simplify solutions.
    useSimplification :: Bool,
    -- | When auto simplifying, max amt items deleted in a single step.
    simplificationMaxAmt :: Int,
    -- | When auto simplifying, number of simplification steps.
    simplificationSteps :: Int,
    -- | When auto simplifying, whether to print verbose information.
    simplificationVerbose :: Bool,
    -- | Whether to use mutli-threading.
    useMultiThreading :: Bool,
    -- | Max total error for solutions.
    solutionErrorThreshold :: Double,
    -- | Limit of push interpreter steps in push program evaluation.
    stepLimit :: Int,
    -- | For tournament selection, amount of individuals in each tournament.
    tournamentSize :: Int,
    -- | Training data for the gp, must be provided.
    trainingData :: [PushData],
    -- | Testing data for the gp, must be provided if there is any.
    testingData :: [PushData],
    -- | Addition rate for UMAD (deletion rate derived from this). Should be an Int [0-100].
    umadRate :: Double,
    -- | Genetic operators and probabilities for their use, should sum to one
    -- Takes a Map of String -> Float where the string is the genetic operator
    variation :: [(String,Double)],
    -- | The epsilons calculated for epsilon lexicase selection. Only used for epsilon lexicase selection.
    epsilons :: Maybe [Double],
    -- | Used with the CaseMaxminAuto downsampling strategy. Tells downsampling to stop when
    -- the maximum minimum distance is too far away.
    caseDelta :: Double,
    -- | Used in lexicase selection. If initialCases is present will use those before randomly
    -- selecting from the population for initial cases. Can raise a value into the IO monad using
    -- `pure @IO`
    initialCases :: Maybe [Int]
  }

-- | The default values for which all runs of Hush derive
-- their args from.
defaultPushArgs :: PushArgs
defaultPushArgs = PushArgs {
    alignmentDeviation = 2.0,
    alternationRate = 0.1,
    bmxExchangeRate = 0.5,
    bmxGeneLengthLimit = 10,
    bmxGapChangeProbability = 0.001,
    bmxIsComplementary = False,
    bmxMaxDistance = 1000000,
    bmxSameGeneCount = False,
    closes = "specified",
    useBMX = False,
    customReport = Nothing,
    dontEnd = False,
    enableDownsampling = True,
    downsampleFunction = "caseMaxim",
    downsampleRate = 0.05,
    downsampleParentRate = 0.01,
    downsampleParentsGens = 10,
    elitism = False,
    errorFunction = error "Must supply the error function yourself",
    informedDownsamplingType = "solved",
    instructionList = allInstructions,
    maxMotelyBatchSize = 10,
    maxInitialPlushySize = 100,
    maxGenerations = 1000,
    parentSelectionAlgo = "lexicase",
    populationSize = 1000,
    replacementRate = 0.1,
    useSimplification = True,
    simplificationMaxAmt = 4,
    simplificationSteps = 1000,
    simplificationVerbose = False,
    useMultiThreading = False, -- False for now, change to True later.
    solutionErrorThreshold = 0.0,
    ssxNotBmx = False,
    stepLimit = 1000,
    tournamentSize = 5,
    testingData = error "Must supply the testingData yourself",
    trainingData = error "Must supply the trainingData yourself",
    umadRate = 0.1,
    variation = [("umad", 1.0)],
    epsilons = Nothing,
    caseDelta = 0,
    initialCases = Nothing
  }
