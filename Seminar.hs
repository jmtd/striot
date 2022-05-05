{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Algebra.Graph
import Test.Framework

import Striot.CompileIoT
import Striot.LogicalOptimiser
import Striot.Jackson
import Striot.Orchestration
import Striot.StreamGraph
import Striot.VizGraph

main = htfMain htf_Main_thisModulesTests

------------------------------------------------------------------------------
-- Example 1: over-utilised Filter operator
-- hoisting filter upstream of Merge resolves

v1 = StreamVertex 1 (Source 1) [[| tempSensor |]] "IO ()" "Int" 1.0 -- util: 1
v2 = StreamVertex 2 (Source 1) [[| tempSensor |]] "IO ()" "Int" 1.0 -- util: 1
v3 = StreamVertex 3 (Source 1) [[| tempSensor |]] "IO ()" "Int" 1.0 -- util: 1

v4 = StreamVertex 4 Merge [] "Int" "Int"                        0.3 -- util: 0.89
v5 = StreamVertex 5 (Filter 0.1) [[| over100 |]] "Int" "Int"    1.0 -- util: 3
v6 = StreamVertex 6 Map [[| f |]] "Int" "String"                0.5 -- util: 0.15
v7 = StreamVertex 7 Sink [] "String" "IO ()"                    0.5 -- util: 0.15

example1 = overlays
  [ path [v1,v4,v5,v6,v7]
  , Vertex v2 `connect` Vertex v4
  , Vertex v3 `connect` Vertex v4
  ]

test_example1_overUtilised = assertBool . isOverUtilised . calcAllSg $ example1

-- mergeFilter matches filter-after-merge and hoists upstream
example1b = applyRule mergeFilter example1
test_example1b_not_overUtilised = assertBool . not . isOverUtilised . calcAllSg $ example1b

------------------------------------------------------------------------------
-- Example 2: discard according to max node util threshold

example2 = path
  [ StreamVertex 1 (Source 1) [[| tempSensor  |]] "IO ()" "Int" 1.0
  , StreamVertex 2 Map        [[| expensiveOp |]] "Int"   "Int" 1.0
  , StreamVertex 3 Map        [[| expensiveOp |]] "Int"   "Int" 1.0
  , StreamVertex 4 Map        [[| expensiveOp |]] "Int"   "Int" 1.0
  , StreamVertex 5 Map        [[| expensiveOp |]] "Int"   "Int" 1.0
  , StreamVertex 6 Map        [[| expensiveOp |]] "Int"   "Int" 1.0
  , StreamVertex 7 Map        [[| expensiveOp |]] "Int"   "Int" 1.0
  , StreamVertex 8 Map        [[| expensiveOp |]] "Int"   "Int" 1.0
  , StreamVertex 9 Sink       [[| mapM_ print |]] "Int" "IO ()" 1.0
  ]

-- deployment plan, disregarding maxNodeUtil: 2 nodes
example2plan = chopAndChange (defaultOpts { bandwidthLimit = 666, maxNodeUtil =1000 }) example2
showExample2 = displayPartitionedGraph . uncurry createPartitions $ example2plan
test_example2plan_numParts = assertEqual 2 $ length . snd $ example2plan

-- deployment plan considering maxNodeUtil=3: 3 nodes
example2planb = chopAndChange (defaultOpts { bandwidthLimit = 666, maxNodeUtil = 3 }) example2
showExample2b = displayPartitionedGraph . uncurry createPartitions $ example2planb
test_example2planb_numParts = assertEqual 3 $ length . snd $ example2planb

------------------------------------------------------------------------------
-- Example 3: Reduce required Cloud nodes by increasing Edge utilisation

