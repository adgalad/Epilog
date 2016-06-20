{-# OPTIONS_GHC -w #-}
module Language.Epilog.Parser
    ( parse
    ) where
--------------------------------------------------------------------------------
import           Language.Epilog.AST.Expression
import           Language.Epilog.AST.Instruction
import           Language.Epilog.Type
import           Language.Epilog.At
import           Language.Epilog.Lexer
import           Language.Epilog.Context
import           Language.Epilog.Epilog
import           Language.Epilog.Error
import           Language.Epilog.SymbolTable
--------------------------------------------------------------------------------
import           Control.Monad.Trans.RWS.Strict (RWS, execRWS, get, gets,
                                                 modify, put, tell)
import           Control.Monad                  (unless)
import           Data.Int                       (Int32)
import           Data.Sequence                  (Seq, ViewL ((:<)), (<|), (><),
                                                 (|>))
import qualified Data.Sequence                  as Seq (empty, singleton, viewl)
import           Prelude                        hiding (Either, lookup)
import           Control.Lens                   ((%=), use, (.=), (+=), (<~))
--------------------------------------------------------------------------------
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25 t26 t27 t28 t29 t30 t31 t32 t33 t34 t35 t36 t37 t38 t39 t40 t41 t42 t43 t44 t45 t46 t47 t48 t49 t50 t51 t52 t53 t54 t55 t56 t57 t58 t59 t60 t61 t62 t63 t64 t65 t66 t67 t68 t69 t70
	= HappyTerminal (At Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25
	| HappyAbsSyn26 t26
	| HappyAbsSyn27 t27
	| HappyAbsSyn28 t28
	| HappyAbsSyn29 t29
	| HappyAbsSyn30 t30
	| HappyAbsSyn31 t31
	| HappyAbsSyn32 t32
	| HappyAbsSyn33 t33
	| HappyAbsSyn34 t34
	| HappyAbsSyn35 t35
	| HappyAbsSyn36 t36
	| HappyAbsSyn37 t37
	| HappyAbsSyn38 t38
	| HappyAbsSyn39 t39
	| HappyAbsSyn40 t40
	| HappyAbsSyn41 t41
	| HappyAbsSyn42 t42
	| HappyAbsSyn43 t43
	| HappyAbsSyn44 t44
	| HappyAbsSyn45 t45
	| HappyAbsSyn46 t46
	| HappyAbsSyn47 t47
	| HappyAbsSyn48 t48
	| HappyAbsSyn49 t49
	| HappyAbsSyn50 t50
	| HappyAbsSyn51 t51
	| HappyAbsSyn52 t52
	| HappyAbsSyn53 t53
	| HappyAbsSyn54 t54
	| HappyAbsSyn55 t55
	| HappyAbsSyn56 t56
	| HappyAbsSyn57 t57
	| HappyAbsSyn58 t58
	| HappyAbsSyn59 t59
	| HappyAbsSyn60 t60
	| HappyAbsSyn61 t61
	| HappyAbsSyn62 t62
	| HappyAbsSyn63 t63
	| HappyAbsSyn64 t64
	| HappyAbsSyn65 t65
	| HappyAbsSyn66 t66
	| HappyAbsSyn67 t67
	| HappyAbsSyn68 t68
	| HappyAbsSyn69 t69
	| HappyAbsSyn70 t70

action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (87) = happyShift action_18
action_2 (111) = happyShift action_19
action_2 (115) = happyShift action_20
action_2 (116) = happyShift action_21
action_2 (121) = happyShift action_22
action_2 (132) = happyShift action_23
action_2 (7) = happyGoto action_4
action_2 (8) = happyGoto action_5
action_2 (9) = happyGoto action_6
action_2 (10) = happyGoto action_7
action_2 (11) = happyGoto action_8
action_2 (12) = happyGoto action_9
action_2 (15) = happyGoto action_10
action_2 (23) = happyGoto action_11
action_2 (24) = happyGoto action_12
action_2 (25) = happyGoto action_13
action_2 (26) = happyGoto action_14
action_2 (27) = happyGoto action_15
action_2 (28) = happyGoto action_16
action_2 (29) = happyGoto action_17
action_2 _ = happyFail

action_3 (133) = happyAccept
action_3 _ = happyFail

action_4 (87) = happyShift action_18
action_4 (111) = happyShift action_19
action_4 (115) = happyShift action_20
action_4 (116) = happyShift action_21
action_4 (121) = happyShift action_22
action_4 (132) = happyShift action_23
action_4 (6) = happyGoto action_37
action_4 (8) = happyGoto action_38
action_4 (9) = happyGoto action_6
action_4 (10) = happyGoto action_7
action_4 (11) = happyGoto action_8
action_4 (12) = happyGoto action_9
action_4 (15) = happyGoto action_10
action_4 (23) = happyGoto action_11
action_4 (24) = happyGoto action_12
action_4 (25) = happyGoto action_13
action_4 (26) = happyGoto action_14
action_4 (27) = happyGoto action_15
action_4 (28) = happyGoto action_16
action_4 (29) = happyGoto action_17
action_4 _ = happyReduce_3

action_5 _ = happyReduce_4

action_6 (112) = happyShift action_36
action_6 _ = happyFail

action_7 (132) = happyShift action_23
action_7 (15) = happyGoto action_35
action_7 _ = happyFail

action_8 _ = happyReduce_8

action_9 (120) = happyShift action_34
action_9 (13) = happyGoto action_33
action_9 _ = happyReduce_15

action_10 _ = happyReduce_47

action_11 (118) = happyShift action_32
action_11 _ = happyFail

action_12 (118) = happyShift action_31
action_12 _ = happyFail

action_13 (131) = happyShift action_30
action_13 (34) = happyGoto action_29
action_13 _ = happyFail

action_14 _ = happyReduce_43

action_15 (30) = happyGoto action_28
action_15 _ = happyReduce_51

action_16 _ = happyReduce_45

action_17 (87) = happyShift action_27
action_17 (121) = happyShift action_22
action_17 (132) = happyShift action_23
action_17 (15) = happyGoto action_10
action_17 (28) = happyGoto action_26
action_17 _ = happyFail

action_18 _ = happyReduce_49

action_19 (132) = happyShift action_23
action_19 (15) = happyGoto action_25
action_19 _ = happyFail

action_20 _ = happyReduce_11

action_21 _ = happyReduce_12

action_22 (87) = happyShift action_18
action_22 (121) = happyShift action_22
action_22 (132) = happyShift action_23
action_22 (15) = happyGoto action_10
action_22 (26) = happyGoto action_24
action_22 (27) = happyGoto action_15
action_22 (28) = happyGoto action_16
action_22 (29) = happyGoto action_17
action_22 _ = happyFail

action_23 _ = happyReduce_18

action_24 (122) = happyShift action_52
action_24 _ = happyFail

action_25 (121) = happyShift action_51
action_25 (69) = happyGoto action_50
action_25 _ = happyFail

action_26 _ = happyReduce_46

action_27 _ = happyReduce_50

action_28 (83) = happyShift action_48
action_28 (85) = happyShift action_49
action_28 (31) = happyGoto action_47
action_28 _ = happyReduce_44

action_29 (123) = happyShift action_46
action_29 _ = happyReduce_41

action_30 _ = happyReduce_63

action_31 _ = happyReduce_7

action_32 _ = happyReduce_6

action_33 (112) = happyShift action_45
action_33 (14) = happyGoto action_43
action_33 (70) = happyGoto action_44
action_33 _ = happyFail

action_34 (87) = happyShift action_18
action_34 (121) = happyShift action_22
action_34 (132) = happyShift action_23
action_34 (15) = happyGoto action_10
action_34 (25) = happyGoto action_42
action_34 (26) = happyGoto action_14
action_34 (27) = happyGoto action_15
action_34 (28) = happyGoto action_16
action_34 (29) = happyGoto action_17
action_34 _ = happyFail

action_35 _ = happyReduce_10

action_36 (87) = happyShift action_18
action_36 (121) = happyShift action_22
action_36 (132) = happyShift action_23
action_36 (15) = happyGoto action_10
action_36 (19) = happyGoto action_39
action_36 (20) = happyGoto action_40
action_36 (25) = happyGoto action_41
action_36 (26) = happyGoto action_14
action_36 (27) = happyGoto action_15
action_36 (28) = happyGoto action_16
action_36 (29) = happyGoto action_17
action_36 _ = happyFail

action_37 _ = happyReduce_1

action_38 _ = happyReduce_5

action_39 (117) = happyShift action_100
action_39 (118) = happyShift action_101
action_39 _ = happyFail

action_40 _ = happyReduce_24

action_41 (131) = happyShift action_30
action_41 (34) = happyGoto action_99
action_41 _ = happyFail

action_42 _ = happyReduce_16

action_43 _ = happyReduce_13

action_44 (87) = happyShift action_18
action_44 (104) = happyShift action_91
action_44 (107) = happyShift action_92
action_44 (108) = happyShift action_93
action_44 (109) = happyShift action_94
action_44 (113) = happyShift action_95
action_44 (114) = happyShift action_96
action_44 (121) = happyShift action_22
action_44 (124) = happyShift action_97
action_44 (125) = happyShift action_98
action_44 (131) = happyShift action_30
action_44 (132) = happyShift action_23
action_44 (15) = happyGoto action_78
action_44 (21) = happyGoto action_79
action_44 (22) = happyGoto action_80
action_44 (23) = happyGoto action_81
action_44 (24) = happyGoto action_82
action_44 (25) = happyGoto action_13
action_44 (26) = happyGoto action_14
action_44 (27) = happyGoto action_15
action_44 (28) = happyGoto action_16
action_44 (29) = happyGoto action_17
action_44 (32) = happyGoto action_83
action_44 (33) = happyGoto action_84
action_44 (34) = happyGoto action_62
action_44 (35) = happyGoto action_85
action_44 (39) = happyGoto action_86
action_44 (43) = happyGoto action_87
action_44 (49) = happyGoto action_88
action_44 (55) = happyGoto action_89
action_44 (68) = happyGoto action_90
action_44 _ = happyFail

action_45 _ = happyReduce_141

action_46 (76) = happyShift action_70
action_46 (79) = happyShift action_71
action_46 (90) = happyShift action_72
action_46 (121) = happyShift action_73
action_46 (126) = happyShift action_74
action_46 (127) = happyShift action_75
action_46 (128) = happyShift action_58
action_46 (129) = happyShift action_76
action_46 (130) = happyShift action_77
action_46 (131) = happyShift action_30
action_46 (132) = happyShift action_23
action_46 (15) = happyGoto action_60
action_46 (33) = happyGoto action_61
action_46 (34) = happyGoto action_62
action_46 (35) = happyGoto action_63
action_46 (56) = happyGoto action_64
action_46 (57) = happyGoto action_65
action_46 (58) = happyGoto action_66
action_46 (59) = happyGoto action_67
action_46 (60) = happyGoto action_68
action_46 (61) = happyGoto action_69
action_46 _ = happyFail

action_47 _ = happyReduce_52

action_48 (128) = happyShift action_58
action_48 (59) = happyGoto action_59
action_48 _ = happyFail

action_49 (128) = happyShift action_58
action_49 (59) = happyGoto action_57
action_49 _ = happyFail

action_50 (87) = happyShift action_18
action_50 (121) = happyShift action_22
action_50 (132) = happyShift action_23
action_50 (15) = happyGoto action_10
action_50 (16) = happyGoto action_53
action_50 (17) = happyGoto action_54
action_50 (18) = happyGoto action_55
action_50 (25) = happyGoto action_56
action_50 (26) = happyGoto action_14
action_50 (27) = happyGoto action_15
action_50 (28) = happyGoto action_16
action_50 (29) = happyGoto action_17
action_50 _ = happyReduce_19

action_51 _ = happyReduce_140

action_52 _ = happyReduce_48

action_53 (122) = happyShift action_159
action_53 _ = happyFail

action_54 (117) = happyShift action_158
action_54 _ = happyReduce_20

action_55 _ = happyReduce_21

action_56 (131) = happyShift action_30
action_56 (34) = happyGoto action_157
action_56 _ = happyFail

action_57 (117) = happyShift action_156
action_57 _ = happyFail

action_58 _ = happyReduce_130

action_59 (86) = happyShift action_154
action_59 (117) = happyShift action_155
action_59 _ = happyFail

action_60 (121) = happyShift action_125
action_60 _ = happyFail

action_61 (83) = happyShift action_117
action_61 (87) = happyShift action_118
action_61 (88) = happyShift action_119
action_61 _ = happyReduce_99

action_62 _ = happyReduce_59

action_63 _ = happyReduce_100

action_64 (71) = happyShift action_130
action_64 (72) = happyShift action_131
action_64 (73) = happyShift action_132
action_64 (74) = happyShift action_133
action_64 (75) = happyShift action_134
action_64 (77) = happyShift action_135
action_64 (78) = happyShift action_136
action_64 (80) = happyShift action_137
action_64 (81) = happyShift action_138
action_64 (82) = happyShift action_139
action_64 (89) = happyShift action_140
action_64 (90) = happyShift action_141
action_64 (91) = happyShift action_142
action_64 (92) = happyShift action_143
action_64 (93) = happyShift action_144
action_64 (94) = happyShift action_145
action_64 (95) = happyShift action_146
action_64 (96) = happyShift action_147
action_64 (97) = happyShift action_148
action_64 (98) = happyShift action_149
action_64 (99) = happyShift action_150
action_64 (100) = happyShift action_151
action_64 (101) = happyShift action_152
action_64 (102) = happyShift action_153
action_64 _ = happyReduce_42

action_65 _ = happyReduce_94

action_66 _ = happyReduce_95

action_67 _ = happyReduce_96

action_68 _ = happyReduce_97

action_69 _ = happyReduce_98

action_70 (76) = happyShift action_70
action_70 (79) = happyShift action_71
action_70 (90) = happyShift action_72
action_70 (121) = happyShift action_73
action_70 (126) = happyShift action_74
action_70 (127) = happyShift action_75
action_70 (128) = happyShift action_58
action_70 (129) = happyShift action_76
action_70 (130) = happyShift action_77
action_70 (131) = happyShift action_30
action_70 (132) = happyShift action_23
action_70 (15) = happyGoto action_60
action_70 (33) = happyGoto action_61
action_70 (34) = happyGoto action_62
action_70 (35) = happyGoto action_63
action_70 (56) = happyGoto action_129
action_70 (57) = happyGoto action_65
action_70 (58) = happyGoto action_66
action_70 (59) = happyGoto action_67
action_70 (60) = happyGoto action_68
action_70 (61) = happyGoto action_69
action_70 _ = happyFail

action_71 (76) = happyShift action_70
action_71 (79) = happyShift action_71
action_71 (90) = happyShift action_72
action_71 (121) = happyShift action_73
action_71 (126) = happyShift action_74
action_71 (127) = happyShift action_75
action_71 (128) = happyShift action_58
action_71 (129) = happyShift action_76
action_71 (130) = happyShift action_77
action_71 (131) = happyShift action_30
action_71 (132) = happyShift action_23
action_71 (15) = happyGoto action_60
action_71 (33) = happyGoto action_61
action_71 (34) = happyGoto action_62
action_71 (35) = happyGoto action_63
action_71 (56) = happyGoto action_128
action_71 (57) = happyGoto action_65
action_71 (58) = happyGoto action_66
action_71 (59) = happyGoto action_67
action_71 (60) = happyGoto action_68
action_71 (61) = happyGoto action_69
action_71 _ = happyFail

action_72 (76) = happyShift action_70
action_72 (79) = happyShift action_71
action_72 (90) = happyShift action_72
action_72 (121) = happyShift action_73
action_72 (126) = happyShift action_74
action_72 (127) = happyShift action_75
action_72 (128) = happyShift action_58
action_72 (129) = happyShift action_76
action_72 (130) = happyShift action_77
action_72 (131) = happyShift action_30
action_72 (132) = happyShift action_23
action_72 (15) = happyGoto action_60
action_72 (33) = happyGoto action_61
action_72 (34) = happyGoto action_62
action_72 (35) = happyGoto action_63
action_72 (56) = happyGoto action_127
action_72 (57) = happyGoto action_65
action_72 (58) = happyGoto action_66
action_72 (59) = happyGoto action_67
action_72 (60) = happyGoto action_68
action_72 (61) = happyGoto action_69
action_72 _ = happyFail

action_73 (76) = happyShift action_70
action_73 (79) = happyShift action_71
action_73 (90) = happyShift action_72
action_73 (121) = happyShift action_73
action_73 (126) = happyShift action_74
action_73 (127) = happyShift action_75
action_73 (128) = happyShift action_58
action_73 (129) = happyShift action_76
action_73 (130) = happyShift action_77
action_73 (131) = happyShift action_30
action_73 (132) = happyShift action_23
action_73 (15) = happyGoto action_60
action_73 (33) = happyGoto action_61
action_73 (34) = happyGoto action_62
action_73 (35) = happyGoto action_63
action_73 (56) = happyGoto action_126
action_73 (57) = happyGoto action_65
action_73 (58) = happyGoto action_66
action_73 (59) = happyGoto action_67
action_73 (60) = happyGoto action_68
action_73 (61) = happyGoto action_69
action_73 _ = happyFail

action_74 _ = happyReduce_128

action_75 _ = happyReduce_129

action_76 _ = happyReduce_131

action_77 _ = happyReduce_132

action_78 (121) = happyShift action_125
action_78 _ = happyReduce_47

action_79 (117) = happyShift action_123
action_79 (118) = happyShift action_124
action_79 (62) = happyGoto action_121
action_79 (64) = happyGoto action_122
action_79 _ = happyFail

action_80 _ = happyReduce_27

action_81 _ = happyReduce_29

action_82 _ = happyReduce_30

action_83 _ = happyReduce_31

action_84 (83) = happyShift action_117
action_84 (87) = happyShift action_118
action_84 (88) = happyShift action_119
action_84 (123) = happyShift action_120
action_84 _ = happyFail

action_85 _ = happyReduce_32

action_86 _ = happyReduce_33

action_87 _ = happyReduce_34

action_88 _ = happyReduce_35

action_89 _ = happyReduce_36

action_90 (87) = happyShift action_18
action_90 (121) = happyShift action_22
action_90 (132) = happyShift action_23
action_90 (15) = happyGoto action_10
action_90 (25) = happyGoto action_115
action_90 (26) = happyGoto action_14
action_90 (27) = happyGoto action_15
action_90 (28) = happyGoto action_16
action_90 (29) = happyGoto action_17
action_90 (50) = happyGoto action_116
action_90 _ = happyFail

action_91 (131) = happyShift action_30
action_91 (34) = happyGoto action_113
action_91 (51) = happyGoto action_114
action_91 _ = happyReduce_139

action_92 (76) = happyShift action_70
action_92 (79) = happyShift action_71
action_92 (90) = happyShift action_72
action_92 (121) = happyShift action_73
action_92 (126) = happyShift action_74
action_92 (127) = happyShift action_75
action_92 (128) = happyShift action_58
action_92 (129) = happyShift action_76
action_92 (130) = happyShift action_77
action_92 (131) = happyShift action_30
action_92 (132) = happyShift action_23
action_92 (15) = happyGoto action_60
action_92 (33) = happyGoto action_61
action_92 (34) = happyGoto action_62
action_92 (35) = happyGoto action_63
action_92 (40) = happyGoto action_112
action_92 (41) = happyGoto action_109
action_92 (42) = happyGoto action_110
action_92 (56) = happyGoto action_111
action_92 (57) = happyGoto action_65
action_92 (58) = happyGoto action_66
action_92 (59) = happyGoto action_67
action_92 (60) = happyGoto action_68
action_92 (61) = happyGoto action_69
action_92 _ = happyFail

action_93 (76) = happyShift action_70
action_93 (79) = happyShift action_71
action_93 (90) = happyShift action_72
action_93 (121) = happyShift action_73
action_93 (126) = happyShift action_74
action_93 (127) = happyShift action_75
action_93 (128) = happyShift action_58
action_93 (129) = happyShift action_76
action_93 (130) = happyShift action_77
action_93 (131) = happyShift action_30
action_93 (132) = happyShift action_23
action_93 (15) = happyGoto action_60
action_93 (33) = happyGoto action_61
action_93 (34) = happyGoto action_62
action_93 (35) = happyGoto action_63
action_93 (40) = happyGoto action_108
action_93 (41) = happyGoto action_109
action_93 (42) = happyGoto action_110
action_93 (56) = happyGoto action_111
action_93 (57) = happyGoto action_65
action_93 (58) = happyGoto action_66
action_93 (59) = happyGoto action_67
action_93 (60) = happyGoto action_68
action_93 (61) = happyGoto action_69
action_93 _ = happyFail

action_94 (76) = happyShift action_70
action_94 (79) = happyShift action_71
action_94 (90) = happyShift action_72
action_94 (121) = happyShift action_73
action_94 (126) = happyShift action_74
action_94 (127) = happyShift action_75
action_94 (128) = happyShift action_58
action_94 (129) = happyShift action_76
action_94 (130) = happyShift action_77
action_94 (131) = happyShift action_30
action_94 (132) = happyShift action_23
action_94 (15) = happyGoto action_60
action_94 (33) = happyGoto action_61
action_94 (34) = happyGoto action_62
action_94 (35) = happyGoto action_63
action_94 (44) = happyGoto action_106
action_94 (56) = happyGoto action_107
action_94 (57) = happyGoto action_65
action_94 (58) = happyGoto action_66
action_94 (59) = happyGoto action_67
action_94 (60) = happyGoto action_68
action_94 (61) = happyGoto action_69
action_94 _ = happyFail

action_95 _ = happyReduce_37

action_96 (76) = happyShift action_70
action_96 (79) = happyShift action_71
action_96 (90) = happyShift action_72
action_96 (121) = happyShift action_73
action_96 (126) = happyShift action_74
action_96 (127) = happyShift action_75
action_96 (128) = happyShift action_58
action_96 (129) = happyShift action_76
action_96 (130) = happyShift action_77
action_96 (131) = happyShift action_30
action_96 (132) = happyShift action_23
action_96 (15) = happyGoto action_60
action_96 (33) = happyGoto action_61
action_96 (34) = happyGoto action_62
action_96 (35) = happyGoto action_63
action_96 (56) = happyGoto action_105
action_96 (57) = happyGoto action_65
action_96 (58) = happyGoto action_66
action_96 (59) = happyGoto action_67
action_96 (60) = happyGoto action_68
action_96 (61) = happyGoto action_69
action_96 _ = happyFail

action_97 (131) = happyShift action_30
action_97 (33) = happyGoto action_104
action_97 (34) = happyGoto action_62
action_97 _ = happyFail

action_98 (76) = happyShift action_70
action_98 (79) = happyShift action_71
action_98 (90) = happyShift action_72
action_98 (121) = happyShift action_73
action_98 (126) = happyShift action_74
action_98 (127) = happyShift action_75
action_98 (128) = happyShift action_58
action_98 (129) = happyShift action_76
action_98 (130) = happyShift action_77
action_98 (131) = happyShift action_30
action_98 (132) = happyShift action_23
action_98 (15) = happyGoto action_60
action_98 (33) = happyGoto action_61
action_98 (34) = happyGoto action_62
action_98 (35) = happyGoto action_63
action_98 (56) = happyGoto action_103
action_98 (57) = happyGoto action_65
action_98 (58) = happyGoto action_66
action_98 (59) = happyGoto action_67
action_98 (60) = happyGoto action_68
action_98 (61) = happyGoto action_69
action_98 _ = happyFail

action_99 _ = happyReduce_26

action_100 (87) = happyShift action_18
action_100 (121) = happyShift action_22
action_100 (132) = happyShift action_23
action_100 (15) = happyGoto action_10
action_100 (20) = happyGoto action_102
action_100 (25) = happyGoto action_41
action_100 (26) = happyGoto action_14
action_100 (27) = happyGoto action_15
action_100 (28) = happyGoto action_16
action_100 (29) = happyGoto action_17
action_100 _ = happyFail

action_101 _ = happyReduce_9

action_102 _ = happyReduce_25

action_103 (71) = happyShift action_130
action_103 (72) = happyShift action_131
action_103 (73) = happyShift action_132
action_103 (74) = happyShift action_133
action_103 (75) = happyShift action_134
action_103 (77) = happyShift action_135
action_103 (78) = happyShift action_136
action_103 (80) = happyShift action_137
action_103 (81) = happyShift action_138
action_103 (82) = happyShift action_139
action_103 (89) = happyShift action_140
action_103 (90) = happyShift action_141
action_103 (91) = happyShift action_142
action_103 (92) = happyShift action_143
action_103 (93) = happyShift action_144
action_103 (94) = happyShift action_145
action_103 (95) = happyShift action_146
action_103 (96) = happyShift action_147
action_103 (97) = happyShift action_148
action_103 (98) = happyShift action_149
action_103 (99) = happyShift action_150
action_103 (100) = happyShift action_151
action_103 (101) = happyShift action_152
action_103 (102) = happyShift action_153
action_103 _ = happyReduce_39

action_104 (83) = happyShift action_117
action_104 (87) = happyShift action_118
action_104 (88) = happyShift action_119
action_104 _ = happyReduce_40

action_105 (71) = happyShift action_130
action_105 (72) = happyShift action_131
action_105 (73) = happyShift action_132
action_105 (74) = happyShift action_133
action_105 (75) = happyShift action_134
action_105 (77) = happyShift action_135
action_105 (78) = happyShift action_136
action_105 (80) = happyShift action_137
action_105 (81) = happyShift action_138
action_105 (82) = happyShift action_139
action_105 (89) = happyShift action_140
action_105 (90) = happyShift action_141
action_105 (91) = happyShift action_142
action_105 (92) = happyShift action_143
action_105 (93) = happyShift action_144
action_105 (94) = happyShift action_145
action_105 (95) = happyShift action_146
action_105 (96) = happyShift action_147
action_105 (97) = happyShift action_148
action_105 (98) = happyShift action_149
action_105 (99) = happyShift action_150
action_105 (100) = happyShift action_151
action_105 (101) = happyShift action_152
action_105 (102) = happyShift action_153
action_105 _ = happyReduce_38

action_106 (110) = happyShift action_209
action_106 _ = happyFail

action_107 (71) = happyShift action_130
action_107 (72) = happyShift action_131
action_107 (73) = happyShift action_132
action_107 (74) = happyShift action_133
action_107 (75) = happyShift action_134
action_107 (77) = happyShift action_135
action_107 (78) = happyShift action_136
action_107 (80) = happyShift action_137
action_107 (81) = happyShift action_138
action_107 (82) = happyShift action_139
action_107 (89) = happyShift action_140
action_107 (90) = happyShift action_141
action_107 (91) = happyShift action_142
action_107 (92) = happyShift action_143
action_107 (93) = happyShift action_144
action_107 (94) = happyShift action_145
action_107 (95) = happyShift action_146
action_107 (96) = happyShift action_147
action_107 (97) = happyShift action_148
action_107 (98) = happyShift action_149
action_107 (99) = happyShift action_150
action_107 (100) = happyShift action_151
action_107 (101) = happyShift action_152
action_107 (102) = happyShift action_153
action_107 _ = happyReduce_76

action_108 (103) = happyShift action_204
action_108 (119) = happyShift action_205
action_108 (63) = happyGoto action_202
action_108 (66) = happyGoto action_208
action_108 _ = happyFail

action_109 _ = happyReduce_71

action_110 (120) = happyShift action_207
action_110 (67) = happyGoto action_206
action_110 _ = happyFail

action_111 (71) = happyShift action_130
action_111 (72) = happyShift action_131
action_111 (73) = happyShift action_132
action_111 (74) = happyShift action_133
action_111 (75) = happyShift action_134
action_111 (77) = happyShift action_135
action_111 (78) = happyShift action_136
action_111 (80) = happyShift action_137
action_111 (81) = happyShift action_138
action_111 (82) = happyShift action_139
action_111 (89) = happyShift action_140
action_111 (90) = happyShift action_141
action_111 (91) = happyShift action_142
action_111 (92) = happyShift action_143
action_111 (93) = happyShift action_144
action_111 (94) = happyShift action_145
action_111 (95) = happyShift action_146
action_111 (96) = happyShift action_147
action_111 (97) = happyShift action_148
action_111 (98) = happyShift action_149
action_111 (99) = happyShift action_150
action_111 (100) = happyShift action_151
action_111 (101) = happyShift action_152
action_111 (102) = happyShift action_153
action_111 _ = happyReduce_74

action_112 (103) = happyShift action_204
action_112 (119) = happyShift action_205
action_112 (63) = happyGoto action_202
action_112 (66) = happyGoto action_203
action_112 _ = happyFail

action_113 _ = happyReduce_87

action_114 (105) = happyShift action_199
action_114 (52) = happyGoto action_201
action_114 (53) = happyGoto action_197
action_114 (54) = happyGoto action_198
action_114 _ = happyFail

action_115 (131) = happyShift action_30
action_115 (34) = happyGoto action_200
action_115 _ = happyFail

action_116 (105) = happyShift action_199
action_116 (52) = happyGoto action_196
action_116 (53) = happyGoto action_197
action_116 (54) = happyGoto action_198
action_116 _ = happyFail

action_117 (76) = happyShift action_70
action_117 (79) = happyShift action_71
action_117 (90) = happyShift action_72
action_117 (121) = happyShift action_73
action_117 (126) = happyShift action_74
action_117 (127) = happyShift action_75
action_117 (128) = happyShift action_58
action_117 (129) = happyShift action_76
action_117 (130) = happyShift action_77
action_117 (131) = happyShift action_30
action_117 (132) = happyShift action_23
action_117 (15) = happyGoto action_60
action_117 (33) = happyGoto action_61
action_117 (34) = happyGoto action_62
action_117 (35) = happyGoto action_63
action_117 (56) = happyGoto action_195
action_117 (57) = happyGoto action_65
action_117 (58) = happyGoto action_66
action_117 (59) = happyGoto action_67
action_117 (60) = happyGoto action_68
action_117 (61) = happyGoto action_69
action_117 _ = happyFail

action_118 _ = happyReduce_62

action_119 (131) = happyShift action_30
action_119 (34) = happyGoto action_194
action_119 _ = happyFail

action_120 (76) = happyShift action_70
action_120 (79) = happyShift action_71
action_120 (90) = happyShift action_72
action_120 (121) = happyShift action_73
action_120 (126) = happyShift action_74
action_120 (127) = happyShift action_75
action_120 (128) = happyShift action_58
action_120 (129) = happyShift action_76
action_120 (130) = happyShift action_77
action_120 (131) = happyShift action_30
action_120 (132) = happyShift action_23
action_120 (15) = happyGoto action_60
action_120 (33) = happyGoto action_61
action_120 (34) = happyGoto action_62
action_120 (35) = happyGoto action_63
action_120 (56) = happyGoto action_193
action_120 (57) = happyGoto action_65
action_120 (58) = happyGoto action_66
action_120 (59) = happyGoto action_67
action_120 (60) = happyGoto action_68
action_120 (61) = happyGoto action_69
action_120 _ = happyFail

action_121 _ = happyReduce_135

action_122 _ = happyReduce_17

action_123 (87) = happyShift action_18
action_123 (104) = happyShift action_91
action_123 (107) = happyShift action_92
action_123 (108) = happyShift action_93
action_123 (109) = happyShift action_94
action_123 (113) = happyShift action_95
action_123 (114) = happyShift action_96
action_123 (121) = happyShift action_22
action_123 (124) = happyShift action_97
action_123 (125) = happyShift action_98
action_123 (131) = happyShift action_30
action_123 (132) = happyShift action_23
action_123 (15) = happyGoto action_78
action_123 (22) = happyGoto action_192
action_123 (23) = happyGoto action_81
action_123 (24) = happyGoto action_82
action_123 (25) = happyGoto action_13
action_123 (26) = happyGoto action_14
action_123 (27) = happyGoto action_15
action_123 (28) = happyGoto action_16
action_123 (29) = happyGoto action_17
action_123 (32) = happyGoto action_83
action_123 (33) = happyGoto action_84
action_123 (34) = happyGoto action_62
action_123 (35) = happyGoto action_85
action_123 (39) = happyGoto action_86
action_123 (43) = happyGoto action_87
action_123 (49) = happyGoto action_88
action_123 (55) = happyGoto action_89
action_123 (68) = happyGoto action_90
action_123 _ = happyFail

action_124 _ = happyReduce_133

action_125 (76) = happyShift action_70
action_125 (79) = happyShift action_71
action_125 (90) = happyShift action_72
action_125 (121) = happyShift action_73
action_125 (126) = happyShift action_74
action_125 (127) = happyShift action_75
action_125 (128) = happyShift action_58
action_125 (129) = happyShift action_76
action_125 (130) = happyShift action_77
action_125 (131) = happyShift action_30
action_125 (132) = happyShift action_23
action_125 (15) = happyGoto action_60
action_125 (33) = happyGoto action_61
action_125 (34) = happyGoto action_62
action_125 (35) = happyGoto action_63
action_125 (36) = happyGoto action_188
action_125 (37) = happyGoto action_189
action_125 (38) = happyGoto action_190
action_125 (56) = happyGoto action_191
action_125 (57) = happyGoto action_65
action_125 (58) = happyGoto action_66
action_125 (59) = happyGoto action_67
action_125 (60) = happyGoto action_68
action_125 (61) = happyGoto action_69
action_125 _ = happyReduce_65

action_126 (71) = happyShift action_130
action_126 (72) = happyShift action_131
action_126 (73) = happyShift action_132
action_126 (74) = happyShift action_133
action_126 (75) = happyShift action_134
action_126 (77) = happyShift action_135
action_126 (78) = happyShift action_136
action_126 (80) = happyShift action_137
action_126 (81) = happyShift action_138
action_126 (82) = happyShift action_139
action_126 (89) = happyShift action_140
action_126 (90) = happyShift action_141
action_126 (91) = happyShift action_142
action_126 (92) = happyShift action_143
action_126 (93) = happyShift action_144
action_126 (94) = happyShift action_145
action_126 (95) = happyShift action_146
action_126 (96) = happyShift action_147
action_126 (97) = happyShift action_148
action_126 (98) = happyShift action_149
action_126 (99) = happyShift action_150
action_126 (100) = happyShift action_151
action_126 (101) = happyShift action_152
action_126 (102) = happyShift action_153
action_126 (122) = happyShift action_187
action_126 _ = happyFail

action_127 _ = happyReduce_119

action_128 _ = happyReduce_112

action_129 _ = happyReduce_106

action_130 (76) = happyShift action_70
action_130 (79) = happyShift action_71
action_130 (90) = happyShift action_72
action_130 (121) = happyShift action_73
action_130 (126) = happyShift action_74
action_130 (127) = happyShift action_75
action_130 (128) = happyShift action_58
action_130 (129) = happyShift action_76
action_130 (130) = happyShift action_77
action_130 (131) = happyShift action_30
action_130 (132) = happyShift action_23
action_130 (15) = happyGoto action_60
action_130 (33) = happyGoto action_61
action_130 (34) = happyGoto action_62
action_130 (35) = happyGoto action_63
action_130 (56) = happyGoto action_186
action_130 (57) = happyGoto action_65
action_130 (58) = happyGoto action_66
action_130 (59) = happyGoto action_67
action_130 (60) = happyGoto action_68
action_130 (61) = happyGoto action_69
action_130 _ = happyFail

action_131 (76) = happyShift action_70
action_131 (79) = happyShift action_71
action_131 (90) = happyShift action_72
action_131 (121) = happyShift action_73
action_131 (126) = happyShift action_74
action_131 (127) = happyShift action_75
action_131 (128) = happyShift action_58
action_131 (129) = happyShift action_76
action_131 (130) = happyShift action_77
action_131 (131) = happyShift action_30
action_131 (132) = happyShift action_23
action_131 (15) = happyGoto action_60
action_131 (33) = happyGoto action_61
action_131 (34) = happyGoto action_62
action_131 (35) = happyGoto action_63
action_131 (56) = happyGoto action_185
action_131 (57) = happyGoto action_65
action_131 (58) = happyGoto action_66
action_131 (59) = happyGoto action_67
action_131 (60) = happyGoto action_68
action_131 (61) = happyGoto action_69
action_131 _ = happyFail

action_132 (76) = happyShift action_70
action_132 (79) = happyShift action_71
action_132 (90) = happyShift action_72
action_132 (121) = happyShift action_73
action_132 (126) = happyShift action_74
action_132 (127) = happyShift action_75
action_132 (128) = happyShift action_58
action_132 (129) = happyShift action_76
action_132 (130) = happyShift action_77
action_132 (131) = happyShift action_30
action_132 (132) = happyShift action_23
action_132 (15) = happyGoto action_60
action_132 (33) = happyGoto action_61
action_132 (34) = happyGoto action_62
action_132 (35) = happyGoto action_63
action_132 (56) = happyGoto action_184
action_132 (57) = happyGoto action_65
action_132 (58) = happyGoto action_66
action_132 (59) = happyGoto action_67
action_132 (60) = happyGoto action_68
action_132 (61) = happyGoto action_69
action_132 _ = happyFail

action_133 (76) = happyShift action_70
action_133 (79) = happyShift action_71
action_133 (90) = happyShift action_72
action_133 (121) = happyShift action_73
action_133 (126) = happyShift action_74
action_133 (127) = happyShift action_75
action_133 (128) = happyShift action_58
action_133 (129) = happyShift action_76
action_133 (130) = happyShift action_77
action_133 (131) = happyShift action_30
action_133 (132) = happyShift action_23
action_133 (15) = happyGoto action_60
action_133 (33) = happyGoto action_61
action_133 (34) = happyGoto action_62
action_133 (35) = happyGoto action_63
action_133 (56) = happyGoto action_183
action_133 (57) = happyGoto action_65
action_133 (58) = happyGoto action_66
action_133 (59) = happyGoto action_67
action_133 (60) = happyGoto action_68
action_133 (61) = happyGoto action_69
action_133 _ = happyFail

action_134 (76) = happyShift action_70
action_134 (79) = happyShift action_71
action_134 (90) = happyShift action_72
action_134 (121) = happyShift action_73
action_134 (126) = happyShift action_74
action_134 (127) = happyShift action_75
action_134 (128) = happyShift action_58
action_134 (129) = happyShift action_76
action_134 (130) = happyShift action_77
action_134 (131) = happyShift action_30
action_134 (132) = happyShift action_23
action_134 (15) = happyGoto action_60
action_134 (33) = happyGoto action_61
action_134 (34) = happyGoto action_62
action_134 (35) = happyGoto action_63
action_134 (56) = happyGoto action_182
action_134 (57) = happyGoto action_65
action_134 (58) = happyGoto action_66
action_134 (59) = happyGoto action_67
action_134 (60) = happyGoto action_68
action_134 (61) = happyGoto action_69
action_134 _ = happyFail

action_135 (76) = happyShift action_70
action_135 (79) = happyShift action_71
action_135 (90) = happyShift action_72
action_135 (121) = happyShift action_73
action_135 (126) = happyShift action_74
action_135 (127) = happyShift action_75
action_135 (128) = happyShift action_58
action_135 (129) = happyShift action_76
action_135 (130) = happyShift action_77
action_135 (131) = happyShift action_30
action_135 (132) = happyShift action_23
action_135 (15) = happyGoto action_60
action_135 (33) = happyGoto action_61
action_135 (34) = happyGoto action_62
action_135 (35) = happyGoto action_63
action_135 (56) = happyGoto action_181
action_135 (57) = happyGoto action_65
action_135 (58) = happyGoto action_66
action_135 (59) = happyGoto action_67
action_135 (60) = happyGoto action_68
action_135 (61) = happyGoto action_69
action_135 _ = happyFail

action_136 (76) = happyShift action_70
action_136 (79) = happyShift action_71
action_136 (90) = happyShift action_72
action_136 (121) = happyShift action_73
action_136 (126) = happyShift action_74
action_136 (127) = happyShift action_75
action_136 (128) = happyShift action_58
action_136 (129) = happyShift action_76
action_136 (130) = happyShift action_77
action_136 (131) = happyShift action_30
action_136 (132) = happyShift action_23
action_136 (15) = happyGoto action_60
action_136 (33) = happyGoto action_61
action_136 (34) = happyGoto action_62
action_136 (35) = happyGoto action_63
action_136 (56) = happyGoto action_180
action_136 (57) = happyGoto action_65
action_136 (58) = happyGoto action_66
action_136 (59) = happyGoto action_67
action_136 (60) = happyGoto action_68
action_136 (61) = happyGoto action_69
action_136 _ = happyFail

action_137 (76) = happyShift action_70
action_137 (79) = happyShift action_71
action_137 (90) = happyShift action_72
action_137 (121) = happyShift action_73
action_137 (126) = happyShift action_74
action_137 (127) = happyShift action_75
action_137 (128) = happyShift action_58
action_137 (129) = happyShift action_76
action_137 (130) = happyShift action_77
action_137 (131) = happyShift action_30
action_137 (132) = happyShift action_23
action_137 (15) = happyGoto action_60
action_137 (33) = happyGoto action_61
action_137 (34) = happyGoto action_62
action_137 (35) = happyGoto action_63
action_137 (56) = happyGoto action_179
action_137 (57) = happyGoto action_65
action_137 (58) = happyGoto action_66
action_137 (59) = happyGoto action_67
action_137 (60) = happyGoto action_68
action_137 (61) = happyGoto action_69
action_137 _ = happyFail

action_138 (76) = happyShift action_70
action_138 (79) = happyShift action_71
action_138 (90) = happyShift action_72
action_138 (121) = happyShift action_73
action_138 (126) = happyShift action_74
action_138 (127) = happyShift action_75
action_138 (128) = happyShift action_58
action_138 (129) = happyShift action_76
action_138 (130) = happyShift action_77
action_138 (131) = happyShift action_30
action_138 (132) = happyShift action_23
action_138 (15) = happyGoto action_60
action_138 (33) = happyGoto action_61
action_138 (34) = happyGoto action_62
action_138 (35) = happyGoto action_63
action_138 (56) = happyGoto action_178
action_138 (57) = happyGoto action_65
action_138 (58) = happyGoto action_66
action_138 (59) = happyGoto action_67
action_138 (60) = happyGoto action_68
action_138 (61) = happyGoto action_69
action_138 _ = happyFail

action_139 (76) = happyShift action_70
action_139 (79) = happyShift action_71
action_139 (90) = happyShift action_72
action_139 (121) = happyShift action_73
action_139 (126) = happyShift action_74
action_139 (127) = happyShift action_75
action_139 (128) = happyShift action_58
action_139 (129) = happyShift action_76
action_139 (130) = happyShift action_77
action_139 (131) = happyShift action_30
action_139 (132) = happyShift action_23
action_139 (15) = happyGoto action_60
action_139 (33) = happyGoto action_61
action_139 (34) = happyGoto action_62
action_139 (35) = happyGoto action_63
action_139 (56) = happyGoto action_177
action_139 (57) = happyGoto action_65
action_139 (58) = happyGoto action_66
action_139 (59) = happyGoto action_67
action_139 (60) = happyGoto action_68
action_139 (61) = happyGoto action_69
action_139 _ = happyFail

action_140 (76) = happyShift action_70
action_140 (79) = happyShift action_71
action_140 (90) = happyShift action_72
action_140 (121) = happyShift action_73
action_140 (126) = happyShift action_74
action_140 (127) = happyShift action_75
action_140 (128) = happyShift action_58
action_140 (129) = happyShift action_76
action_140 (130) = happyShift action_77
action_140 (131) = happyShift action_30
action_140 (132) = happyShift action_23
action_140 (15) = happyGoto action_60
action_140 (33) = happyGoto action_61
action_140 (34) = happyGoto action_62
action_140 (35) = happyGoto action_63
action_140 (56) = happyGoto action_176
action_140 (57) = happyGoto action_65
action_140 (58) = happyGoto action_66
action_140 (59) = happyGoto action_67
action_140 (60) = happyGoto action_68
action_140 (61) = happyGoto action_69
action_140 _ = happyFail

action_141 (76) = happyShift action_70
action_141 (79) = happyShift action_71
action_141 (90) = happyShift action_72
action_141 (121) = happyShift action_73
action_141 (126) = happyShift action_74
action_141 (127) = happyShift action_75
action_141 (128) = happyShift action_58
action_141 (129) = happyShift action_76
action_141 (130) = happyShift action_77
action_141 (131) = happyShift action_30
action_141 (132) = happyShift action_23
action_141 (15) = happyGoto action_60
action_141 (33) = happyGoto action_61
action_141 (34) = happyGoto action_62
action_141 (35) = happyGoto action_63
action_141 (56) = happyGoto action_175
action_141 (57) = happyGoto action_65
action_141 (58) = happyGoto action_66
action_141 (59) = happyGoto action_67
action_141 (60) = happyGoto action_68
action_141 (61) = happyGoto action_69
action_141 _ = happyFail

action_142 (76) = happyShift action_70
action_142 (79) = happyShift action_71
action_142 (90) = happyShift action_72
action_142 (121) = happyShift action_73
action_142 (126) = happyShift action_74
action_142 (127) = happyShift action_75
action_142 (128) = happyShift action_58
action_142 (129) = happyShift action_76
action_142 (130) = happyShift action_77
action_142 (131) = happyShift action_30
action_142 (132) = happyShift action_23
action_142 (15) = happyGoto action_60
action_142 (33) = happyGoto action_61
action_142 (34) = happyGoto action_62
action_142 (35) = happyGoto action_63
action_142 (56) = happyGoto action_174
action_142 (57) = happyGoto action_65
action_142 (58) = happyGoto action_66
action_142 (59) = happyGoto action_67
action_142 (60) = happyGoto action_68
action_142 (61) = happyGoto action_69
action_142 _ = happyFail

action_143 (76) = happyShift action_70
action_143 (79) = happyShift action_71
action_143 (90) = happyShift action_72
action_143 (121) = happyShift action_73
action_143 (126) = happyShift action_74
action_143 (127) = happyShift action_75
action_143 (128) = happyShift action_58
action_143 (129) = happyShift action_76
action_143 (130) = happyShift action_77
action_143 (131) = happyShift action_30
action_143 (132) = happyShift action_23
action_143 (15) = happyGoto action_60
action_143 (33) = happyGoto action_61
action_143 (34) = happyGoto action_62
action_143 (35) = happyGoto action_63
action_143 (56) = happyGoto action_173
action_143 (57) = happyGoto action_65
action_143 (58) = happyGoto action_66
action_143 (59) = happyGoto action_67
action_143 (60) = happyGoto action_68
action_143 (61) = happyGoto action_69
action_143 _ = happyFail

action_144 (76) = happyShift action_70
action_144 (79) = happyShift action_71
action_144 (90) = happyShift action_72
action_144 (121) = happyShift action_73
action_144 (126) = happyShift action_74
action_144 (127) = happyShift action_75
action_144 (128) = happyShift action_58
action_144 (129) = happyShift action_76
action_144 (130) = happyShift action_77
action_144 (131) = happyShift action_30
action_144 (132) = happyShift action_23
action_144 (15) = happyGoto action_60
action_144 (33) = happyGoto action_61
action_144 (34) = happyGoto action_62
action_144 (35) = happyGoto action_63
action_144 (56) = happyGoto action_172
action_144 (57) = happyGoto action_65
action_144 (58) = happyGoto action_66
action_144 (59) = happyGoto action_67
action_144 (60) = happyGoto action_68
action_144 (61) = happyGoto action_69
action_144 _ = happyFail

action_145 (76) = happyShift action_70
action_145 (79) = happyShift action_71
action_145 (90) = happyShift action_72
action_145 (121) = happyShift action_73
action_145 (126) = happyShift action_74
action_145 (127) = happyShift action_75
action_145 (128) = happyShift action_58
action_145 (129) = happyShift action_76
action_145 (130) = happyShift action_77
action_145 (131) = happyShift action_30
action_145 (132) = happyShift action_23
action_145 (15) = happyGoto action_60
action_145 (33) = happyGoto action_61
action_145 (34) = happyGoto action_62
action_145 (35) = happyGoto action_63
action_145 (56) = happyGoto action_171
action_145 (57) = happyGoto action_65
action_145 (58) = happyGoto action_66
action_145 (59) = happyGoto action_67
action_145 (60) = happyGoto action_68
action_145 (61) = happyGoto action_69
action_145 _ = happyFail

action_146 (76) = happyShift action_70
action_146 (79) = happyShift action_71
action_146 (90) = happyShift action_72
action_146 (121) = happyShift action_73
action_146 (126) = happyShift action_74
action_146 (127) = happyShift action_75
action_146 (128) = happyShift action_58
action_146 (129) = happyShift action_76
action_146 (130) = happyShift action_77
action_146 (131) = happyShift action_30
action_146 (132) = happyShift action_23
action_146 (15) = happyGoto action_60
action_146 (33) = happyGoto action_61
action_146 (34) = happyGoto action_62
action_146 (35) = happyGoto action_63
action_146 (56) = happyGoto action_170
action_146 (57) = happyGoto action_65
action_146 (58) = happyGoto action_66
action_146 (59) = happyGoto action_67
action_146 (60) = happyGoto action_68
action_146 (61) = happyGoto action_69
action_146 _ = happyFail

action_147 (76) = happyShift action_70
action_147 (79) = happyShift action_71
action_147 (90) = happyShift action_72
action_147 (121) = happyShift action_73
action_147 (126) = happyShift action_74
action_147 (127) = happyShift action_75
action_147 (128) = happyShift action_58
action_147 (129) = happyShift action_76
action_147 (130) = happyShift action_77
action_147 (131) = happyShift action_30
action_147 (132) = happyShift action_23
action_147 (15) = happyGoto action_60
action_147 (33) = happyGoto action_61
action_147 (34) = happyGoto action_62
action_147 (35) = happyGoto action_63
action_147 (56) = happyGoto action_169
action_147 (57) = happyGoto action_65
action_147 (58) = happyGoto action_66
action_147 (59) = happyGoto action_67
action_147 (60) = happyGoto action_68
action_147 (61) = happyGoto action_69
action_147 _ = happyFail

action_148 (76) = happyShift action_70
action_148 (79) = happyShift action_71
action_148 (90) = happyShift action_72
action_148 (121) = happyShift action_73
action_148 (126) = happyShift action_74
action_148 (127) = happyShift action_75
action_148 (128) = happyShift action_58
action_148 (129) = happyShift action_76
action_148 (130) = happyShift action_77
action_148 (131) = happyShift action_30
action_148 (132) = happyShift action_23
action_148 (15) = happyGoto action_60
action_148 (33) = happyGoto action_61
action_148 (34) = happyGoto action_62
action_148 (35) = happyGoto action_63
action_148 (56) = happyGoto action_168
action_148 (57) = happyGoto action_65
action_148 (58) = happyGoto action_66
action_148 (59) = happyGoto action_67
action_148 (60) = happyGoto action_68
action_148 (61) = happyGoto action_69
action_148 _ = happyFail

action_149 (76) = happyShift action_70
action_149 (79) = happyShift action_71
action_149 (90) = happyShift action_72
action_149 (121) = happyShift action_73
action_149 (126) = happyShift action_74
action_149 (127) = happyShift action_75
action_149 (128) = happyShift action_58
action_149 (129) = happyShift action_76
action_149 (130) = happyShift action_77
action_149 (131) = happyShift action_30
action_149 (132) = happyShift action_23
action_149 (15) = happyGoto action_60
action_149 (33) = happyGoto action_61
action_149 (34) = happyGoto action_62
action_149 (35) = happyGoto action_63
action_149 (56) = happyGoto action_167
action_149 (57) = happyGoto action_65
action_149 (58) = happyGoto action_66
action_149 (59) = happyGoto action_67
action_149 (60) = happyGoto action_68
action_149 (61) = happyGoto action_69
action_149 _ = happyFail

action_150 (76) = happyShift action_70
action_150 (79) = happyShift action_71
action_150 (90) = happyShift action_72
action_150 (121) = happyShift action_73
action_150 (126) = happyShift action_74
action_150 (127) = happyShift action_75
action_150 (128) = happyShift action_58
action_150 (129) = happyShift action_76
action_150 (130) = happyShift action_77
action_150 (131) = happyShift action_30
action_150 (132) = happyShift action_23
action_150 (15) = happyGoto action_60
action_150 (33) = happyGoto action_61
action_150 (34) = happyGoto action_62
action_150 (35) = happyGoto action_63
action_150 (56) = happyGoto action_166
action_150 (57) = happyGoto action_65
action_150 (58) = happyGoto action_66
action_150 (59) = happyGoto action_67
action_150 (60) = happyGoto action_68
action_150 (61) = happyGoto action_69
action_150 _ = happyFail

action_151 (76) = happyShift action_70
action_151 (79) = happyShift action_71
action_151 (90) = happyShift action_72
action_151 (121) = happyShift action_73
action_151 (126) = happyShift action_74
action_151 (127) = happyShift action_75
action_151 (128) = happyShift action_58
action_151 (129) = happyShift action_76
action_151 (130) = happyShift action_77
action_151 (131) = happyShift action_30
action_151 (132) = happyShift action_23
action_151 (15) = happyGoto action_60
action_151 (33) = happyGoto action_61
action_151 (34) = happyGoto action_62
action_151 (35) = happyGoto action_63
action_151 (56) = happyGoto action_165
action_151 (57) = happyGoto action_65
action_151 (58) = happyGoto action_66
action_151 (59) = happyGoto action_67
action_151 (60) = happyGoto action_68
action_151 (61) = happyGoto action_69
action_151 _ = happyFail

action_152 (76) = happyShift action_70
action_152 (79) = happyShift action_71
action_152 (90) = happyShift action_72
action_152 (121) = happyShift action_73
action_152 (126) = happyShift action_74
action_152 (127) = happyShift action_75
action_152 (128) = happyShift action_58
action_152 (129) = happyShift action_76
action_152 (130) = happyShift action_77
action_152 (131) = happyShift action_30
action_152 (132) = happyShift action_23
action_152 (15) = happyGoto action_60
action_152 (33) = happyGoto action_61
action_152 (34) = happyGoto action_62
action_152 (35) = happyGoto action_63
action_152 (56) = happyGoto action_164
action_152 (57) = happyGoto action_65
action_152 (58) = happyGoto action_66
action_152 (59) = happyGoto action_67
action_152 (60) = happyGoto action_68
action_152 (61) = happyGoto action_69
action_152 _ = happyFail

action_153 (76) = happyShift action_70
action_153 (79) = happyShift action_71
action_153 (90) = happyShift action_72
action_153 (121) = happyShift action_73
action_153 (126) = happyShift action_74
action_153 (127) = happyShift action_75
action_153 (128) = happyShift action_58
action_153 (129) = happyShift action_76
action_153 (130) = happyShift action_77
action_153 (131) = happyShift action_30
action_153 (132) = happyShift action_23
action_153 (15) = happyGoto action_60
action_153 (33) = happyGoto action_61
action_153 (34) = happyGoto action_62
action_153 (35) = happyGoto action_63
action_153 (56) = happyGoto action_163
action_153 (57) = happyGoto action_65
action_153 (58) = happyGoto action_66
action_153 (59) = happyGoto action_67
action_153 (60) = happyGoto action_68
action_153 (61) = happyGoto action_69
action_153 _ = happyFail

action_154 _ = happyReduce_53

action_155 (128) = happyShift action_58
action_155 (59) = happyGoto action_162
action_155 _ = happyFail

action_156 (128) = happyShift action_58
action_156 (59) = happyGoto action_161
action_156 _ = happyFail

action_157 _ = happyReduce_23

action_158 (87) = happyShift action_18
action_158 (121) = happyShift action_22
action_158 (132) = happyShift action_23
action_158 (15) = happyGoto action_10
action_158 (18) = happyGoto action_160
action_158 (25) = happyGoto action_56
action_158 (26) = happyGoto action_14
action_158 (27) = happyGoto action_15
action_158 (28) = happyGoto action_16
action_158 (29) = happyGoto action_17
action_158 _ = happyFail

action_159 _ = happyReduce_14

action_160 _ = happyReduce_22

action_161 (84) = happyShift action_229
action_161 (86) = happyShift action_230
action_161 _ = happyFail

action_162 (84) = happyShift action_227
action_162 (86) = happyShift action_228
action_162 _ = happyFail

action_163 (80) = happyShift action_137
action_163 (81) = happyShift action_138
action_163 (89) = happyShift action_140
action_163 (90) = happyShift action_141
action_163 (91) = happyShift action_142
action_163 (92) = happyShift action_143
action_163 (93) = happyShift action_144
action_163 (94) = happyShift action_145
action_163 (95) = happyShift action_146
action_163 (96) = happyShift action_147
action_163 (97) = happyShift action_148
action_163 (98) = happyShift action_149
action_163 (101) = happyFail
action_163 (102) = happyFail
action_163 _ = happyReduce_127

action_164 (80) = happyShift action_137
action_164 (81) = happyShift action_138
action_164 (89) = happyShift action_140
action_164 (90) = happyShift action_141
action_164 (91) = happyShift action_142
action_164 (92) = happyShift action_143
action_164 (93) = happyShift action_144
action_164 (94) = happyShift action_145
action_164 (95) = happyShift action_146
action_164 (96) = happyShift action_147
action_164 (97) = happyShift action_148
action_164 (98) = happyShift action_149
action_164 (101) = happyFail
action_164 (102) = happyFail
action_164 _ = happyReduce_126

action_165 (80) = happyShift action_137
action_165 (81) = happyShift action_138
action_165 (89) = happyShift action_140
action_165 (90) = happyShift action_141
action_165 (91) = happyShift action_142
action_165 (92) = happyShift action_143
action_165 (93) = happyShift action_144
action_165 (94) = happyShift action_145
action_165 (95) = happyShift action_146
action_165 (96) = happyShift action_147
action_165 (97) = happyShift action_148
action_165 (98) = happyShift action_149
action_165 (101) = happyShift action_152
action_165 (102) = happyShift action_153
action_165 _ = happyReduce_125

action_166 (80) = happyShift action_137
action_166 (81) = happyShift action_138
action_166 (89) = happyShift action_140
action_166 (90) = happyShift action_141
action_166 (91) = happyShift action_142
action_166 (92) = happyShift action_143
action_166 (93) = happyShift action_144
action_166 (94) = happyShift action_145
action_166 (95) = happyShift action_146
action_166 (96) = happyShift action_147
action_166 (97) = happyShift action_148
action_166 (98) = happyShift action_149
action_166 (101) = happyShift action_152
action_166 (102) = happyShift action_153
action_166 _ = happyReduce_124

action_167 (80) = happyShift action_137
action_167 (81) = happyShift action_138
action_167 (89) = happyShift action_140
action_167 (90) = happyShift action_141
action_167 (91) = happyShift action_142
action_167 (92) = happyShift action_143
action_167 (93) = happyShift action_144
action_167 (94) = happyShift action_145
action_167 (95) = happyFail
action_167 (96) = happyFail
action_167 (97) = happyFail
action_167 (98) = happyFail
action_167 _ = happyReduce_123

action_168 (80) = happyShift action_137
action_168 (81) = happyShift action_138
action_168 (89) = happyShift action_140
action_168 (90) = happyShift action_141
action_168 (91) = happyShift action_142
action_168 (92) = happyShift action_143
action_168 (93) = happyShift action_144
action_168 (94) = happyShift action_145
action_168 (95) = happyFail
action_168 (96) = happyFail
action_168 (97) = happyFail
action_168 (98) = happyFail
action_168 _ = happyReduce_122

action_169 (80) = happyShift action_137
action_169 (81) = happyShift action_138
action_169 (89) = happyShift action_140
action_169 (90) = happyShift action_141
action_169 (91) = happyShift action_142
action_169 (92) = happyShift action_143
action_169 (93) = happyShift action_144
action_169 (94) = happyShift action_145
action_169 (95) = happyFail
action_169 (96) = happyFail
action_169 (97) = happyFail
action_169 (98) = happyFail
action_169 _ = happyReduce_121

action_170 (80) = happyShift action_137
action_170 (81) = happyShift action_138
action_170 (89) = happyShift action_140
action_170 (90) = happyShift action_141
action_170 (91) = happyShift action_142
action_170 (92) = happyShift action_143
action_170 (93) = happyShift action_144
action_170 (94) = happyShift action_145
action_170 (95) = happyFail
action_170 (96) = happyFail
action_170 (97) = happyFail
action_170 (98) = happyFail
action_170 _ = happyReduce_120

action_171 _ = happyReduce_118

action_172 _ = happyReduce_117

action_173 _ = happyReduce_116

action_174 _ = happyReduce_115

action_175 (91) = happyShift action_142
action_175 (92) = happyShift action_143
action_175 (93) = happyShift action_144
action_175 (94) = happyShift action_145
action_175 _ = happyReduce_114

action_176 (91) = happyShift action_142
action_176 (92) = happyShift action_143
action_176 (93) = happyShift action_144
action_176 (94) = happyShift action_145
action_176 _ = happyReduce_113

action_177 (77) = happyShift action_135
action_177 (80) = happyShift action_137
action_177 (81) = happyShift action_138
action_177 (89) = happyShift action_140
action_177 (90) = happyShift action_141
action_177 (91) = happyShift action_142
action_177 (92) = happyShift action_143
action_177 (93) = happyShift action_144
action_177 (94) = happyShift action_145
action_177 (95) = happyShift action_146
action_177 (96) = happyShift action_147
action_177 (97) = happyShift action_148
action_177 (98) = happyShift action_149
action_177 (99) = happyShift action_150
action_177 (100) = happyShift action_151
action_177 (101) = happyShift action_152
action_177 (102) = happyShift action_153
action_177 _ = happyReduce_111

action_178 (89) = happyShift action_140
action_178 (90) = happyShift action_141
action_178 (91) = happyShift action_142
action_178 (92) = happyShift action_143
action_178 (93) = happyShift action_144
action_178 (94) = happyShift action_145
action_178 _ = happyReduce_110

action_179 (89) = happyShift action_140
action_179 (90) = happyShift action_141
action_179 (91) = happyShift action_142
action_179 (92) = happyShift action_143
action_179 (93) = happyShift action_144
action_179 (94) = happyShift action_145
action_179 _ = happyReduce_109

action_180 (77) = happyShift action_135
action_180 (80) = happyShift action_137
action_180 (81) = happyShift action_138
action_180 (82) = happyShift action_139
action_180 (89) = happyShift action_140
action_180 (90) = happyShift action_141
action_180 (91) = happyShift action_142
action_180 (92) = happyShift action_143
action_180 (93) = happyShift action_144
action_180 (94) = happyShift action_145
action_180 (95) = happyShift action_146
action_180 (96) = happyShift action_147
action_180 (97) = happyShift action_148
action_180 (98) = happyShift action_149
action_180 (99) = happyShift action_150
action_180 (100) = happyShift action_151
action_180 (101) = happyShift action_152
action_180 (102) = happyShift action_153
action_180 _ = happyReduce_108

action_181 (80) = happyShift action_137
action_181 (81) = happyShift action_138
action_181 (89) = happyShift action_140
action_181 (90) = happyShift action_141
action_181 (91) = happyShift action_142
action_181 (92) = happyShift action_143
action_181 (93) = happyShift action_144
action_181 (94) = happyShift action_145
action_181 (95) = happyShift action_146
action_181 (96) = happyShift action_147
action_181 (97) = happyShift action_148
action_181 (98) = happyShift action_149
action_181 (99) = happyShift action_150
action_181 (100) = happyShift action_151
action_181 (101) = happyShift action_152
action_181 (102) = happyShift action_153
action_181 _ = happyReduce_107

action_182 (71) = happyShift action_130
action_182 (77) = happyShift action_135
action_182 (78) = happyShift action_136
action_182 (80) = happyShift action_137
action_182 (81) = happyShift action_138
action_182 (82) = happyShift action_139
action_182 (89) = happyShift action_140
action_182 (90) = happyShift action_141
action_182 (91) = happyShift action_142
action_182 (92) = happyShift action_143
action_182 (93) = happyShift action_144
action_182 (94) = happyShift action_145
action_182 (95) = happyShift action_146
action_182 (96) = happyShift action_147
action_182 (97) = happyShift action_148
action_182 (98) = happyShift action_149
action_182 (99) = happyShift action_150
action_182 (100) = happyShift action_151
action_182 (101) = happyShift action_152
action_182 (102) = happyShift action_153
action_182 _ = happyReduce_105

action_183 (71) = happyShift action_130
action_183 (72) = happyShift action_131
action_183 (73) = happyShift action_132
action_183 (75) = happyShift action_134
action_183 (77) = happyShift action_135
action_183 (78) = happyShift action_136
action_183 (80) = happyShift action_137
action_183 (81) = happyShift action_138
action_183 (82) = happyShift action_139
action_183 (89) = happyShift action_140
action_183 (90) = happyShift action_141
action_183 (91) = happyShift action_142
action_183 (92) = happyShift action_143
action_183 (93) = happyShift action_144
action_183 (94) = happyShift action_145
action_183 (95) = happyShift action_146
action_183 (96) = happyShift action_147
action_183 (97) = happyShift action_148
action_183 (98) = happyShift action_149
action_183 (99) = happyShift action_150
action_183 (100) = happyShift action_151
action_183 (101) = happyShift action_152
action_183 (102) = happyShift action_153
action_183 _ = happyReduce_104

action_184 (71) = happyShift action_130
action_184 (75) = happyShift action_134
action_184 (77) = happyShift action_135
action_184 (78) = happyShift action_136
action_184 (80) = happyShift action_137
action_184 (81) = happyShift action_138
action_184 (82) = happyShift action_139
action_184 (89) = happyShift action_140
action_184 (90) = happyShift action_141
action_184 (91) = happyShift action_142
action_184 (92) = happyShift action_143
action_184 (93) = happyShift action_144
action_184 (94) = happyShift action_145
action_184 (95) = happyShift action_146
action_184 (96) = happyShift action_147
action_184 (97) = happyShift action_148
action_184 (98) = happyShift action_149
action_184 (99) = happyShift action_150
action_184 (100) = happyShift action_151
action_184 (101) = happyShift action_152
action_184 (102) = happyShift action_153
action_184 _ = happyReduce_103

action_185 (71) = happyShift action_130
action_185 (73) = happyShift action_132
action_185 (75) = happyShift action_134
action_185 (77) = happyShift action_135
action_185 (78) = happyShift action_136
action_185 (80) = happyShift action_137
action_185 (81) = happyShift action_138
action_185 (82) = happyShift action_139
action_185 (89) = happyShift action_140
action_185 (90) = happyShift action_141
action_185 (91) = happyShift action_142
action_185 (92) = happyShift action_143
action_185 (93) = happyShift action_144
action_185 (94) = happyShift action_145
action_185 (95) = happyShift action_146
action_185 (96) = happyShift action_147
action_185 (97) = happyShift action_148
action_185 (98) = happyShift action_149
action_185 (99) = happyShift action_150
action_185 (100) = happyShift action_151
action_185 (101) = happyShift action_152
action_185 (102) = happyShift action_153
action_185 _ = happyReduce_102

action_186 (77) = happyShift action_135
action_186 (78) = happyShift action_136
action_186 (80) = happyShift action_137
action_186 (81) = happyShift action_138
action_186 (82) = happyShift action_139
action_186 (89) = happyShift action_140
action_186 (90) = happyShift action_141
action_186 (91) = happyShift action_142
action_186 (92) = happyShift action_143
action_186 (93) = happyShift action_144
action_186 (94) = happyShift action_145
action_186 (95) = happyShift action_146
action_186 (96) = happyShift action_147
action_186 (97) = happyShift action_148
action_186 (98) = happyShift action_149
action_186 (99) = happyShift action_150
action_186 (100) = happyShift action_151
action_186 (101) = happyShift action_152
action_186 (102) = happyShift action_153
action_186 _ = happyReduce_101

action_187 _ = happyReduce_93

action_188 (122) = happyShift action_226
action_188 _ = happyFail

action_189 (117) = happyShift action_225
action_189 _ = happyReduce_66

action_190 _ = happyReduce_67

action_191 (71) = happyShift action_130
action_191 (72) = happyShift action_131
action_191 (73) = happyShift action_132
action_191 (74) = happyShift action_133
action_191 (75) = happyShift action_134
action_191 (77) = happyShift action_135
action_191 (78) = happyShift action_136
action_191 (80) = happyShift action_137
action_191 (81) = happyShift action_138
action_191 (82) = happyShift action_139
action_191 (89) = happyShift action_140
action_191 (90) = happyShift action_141
action_191 (91) = happyShift action_142
action_191 (92) = happyShift action_143
action_191 (93) = happyShift action_144
action_191 (94) = happyShift action_145
action_191 (95) = happyShift action_146
action_191 (96) = happyShift action_147
action_191 (97) = happyShift action_148
action_191 (98) = happyShift action_149
action_191 (99) = happyShift action_150
action_191 (100) = happyShift action_151
action_191 (101) = happyShift action_152
action_191 (102) = happyShift action_153
action_191 _ = happyReduce_69

action_192 _ = happyReduce_28

action_193 (71) = happyShift action_130
action_193 (72) = happyShift action_131
action_193 (73) = happyShift action_132
action_193 (74) = happyShift action_133
action_193 (75) = happyShift action_134
action_193 (77) = happyShift action_135
action_193 (78) = happyShift action_136
action_193 (80) = happyShift action_137
action_193 (81) = happyShift action_138
action_193 (82) = happyShift action_139
action_193 (89) = happyShift action_140
action_193 (90) = happyShift action_141
action_193 (91) = happyShift action_142
action_193 (92) = happyShift action_143
action_193 (93) = happyShift action_144
action_193 (94) = happyShift action_145
action_193 (95) = happyShift action_146
action_193 (96) = happyShift action_147
action_193 (97) = happyShift action_148
action_193 (98) = happyShift action_149
action_193 (99) = happyShift action_150
action_193 (100) = happyShift action_151
action_193 (101) = happyShift action_152
action_193 (102) = happyShift action_153
action_193 _ = happyReduce_58

action_194 _ = happyReduce_61

action_195 (71) = happyShift action_130
action_195 (72) = happyShift action_131
action_195 (73) = happyShift action_132
action_195 (74) = happyShift action_133
action_195 (75) = happyShift action_134
action_195 (77) = happyShift action_135
action_195 (78) = happyShift action_136
action_195 (80) = happyShift action_137
action_195 (81) = happyShift action_138
action_195 (82) = happyShift action_139
action_195 (84) = happyShift action_224
action_195 (89) = happyShift action_140
action_195 (90) = happyShift action_141
action_195 (91) = happyShift action_142
action_195 (92) = happyShift action_143
action_195 (93) = happyShift action_144
action_195 (94) = happyShift action_145
action_195 (95) = happyShift action_146
action_195 (96) = happyShift action_147
action_195 (97) = happyShift action_148
action_195 (98) = happyShift action_149
action_195 (99) = happyShift action_150
action_195 (100) = happyShift action_151
action_195 (101) = happyShift action_152
action_195 (102) = happyShift action_153
action_195 _ = happyFail

action_196 (103) = happyShift action_204
action_196 (119) = happyShift action_205
action_196 (63) = happyGoto action_218
action_196 (65) = happyGoto action_222
action_196 (66) = happyGoto action_223
action_196 _ = happyFail

action_197 _ = happyReduce_88

action_198 (120) = happyShift action_207
action_198 (67) = happyGoto action_221
action_198 _ = happyFail

action_199 (76) = happyShift action_70
action_199 (79) = happyShift action_71
action_199 (90) = happyShift action_72
action_199 (121) = happyShift action_73
action_199 (126) = happyShift action_74
action_199 (127) = happyShift action_75
action_199 (128) = happyShift action_58
action_199 (129) = happyShift action_76
action_199 (130) = happyShift action_77
action_199 (131) = happyShift action_30
action_199 (132) = happyShift action_23
action_199 (15) = happyGoto action_60
action_199 (33) = happyGoto action_61
action_199 (34) = happyGoto action_62
action_199 (35) = happyGoto action_63
action_199 (56) = happyGoto action_220
action_199 (57) = happyGoto action_65
action_199 (58) = happyGoto action_66
action_199 (59) = happyGoto action_67
action_199 (60) = happyGoto action_68
action_199 (61) = happyGoto action_69
action_199 _ = happyFail

action_200 _ = happyReduce_86

action_201 (103) = happyShift action_204
action_201 (119) = happyShift action_205
action_201 (63) = happyGoto action_218
action_201 (66) = happyGoto action_219
action_201 _ = happyFail

action_202 (76) = happyShift action_70
action_202 (79) = happyShift action_71
action_202 (90) = happyShift action_72
action_202 (121) = happyShift action_73
action_202 (126) = happyShift action_74
action_202 (127) = happyShift action_75
action_202 (128) = happyShift action_58
action_202 (129) = happyShift action_76
action_202 (130) = happyShift action_77
action_202 (131) = happyShift action_30
action_202 (132) = happyShift action_23
action_202 (15) = happyGoto action_60
action_202 (33) = happyGoto action_61
action_202 (34) = happyGoto action_62
action_202 (35) = happyGoto action_63
action_202 (41) = happyGoto action_217
action_202 (42) = happyGoto action_110
action_202 (56) = happyGoto action_111
action_202 (57) = happyGoto action_65
action_202 (58) = happyGoto action_66
action_202 (59) = happyGoto action_67
action_202 (60) = happyGoto action_68
action_202 (61) = happyGoto action_69
action_202 _ = happyFail

action_203 _ = happyReduce_70

action_204 _ = happyReduce_137

action_205 _ = happyReduce_134

action_206 (87) = happyShift action_18
action_206 (104) = happyShift action_91
action_206 (107) = happyShift action_92
action_206 (108) = happyShift action_93
action_206 (109) = happyShift action_94
action_206 (113) = happyShift action_95
action_206 (114) = happyShift action_96
action_206 (121) = happyShift action_22
action_206 (124) = happyShift action_97
action_206 (125) = happyShift action_98
action_206 (131) = happyShift action_30
action_206 (132) = happyShift action_23
action_206 (15) = happyGoto action_78
action_206 (21) = happyGoto action_216
action_206 (22) = happyGoto action_80
action_206 (23) = happyGoto action_81
action_206 (24) = happyGoto action_82
action_206 (25) = happyGoto action_13
action_206 (26) = happyGoto action_14
action_206 (27) = happyGoto action_15
action_206 (28) = happyGoto action_16
action_206 (29) = happyGoto action_17
action_206 (32) = happyGoto action_83
action_206 (33) = happyGoto action_84
action_206 (34) = happyGoto action_62
action_206 (35) = happyGoto action_85
action_206 (39) = happyGoto action_86
action_206 (43) = happyGoto action_87
action_206 (49) = happyGoto action_88
action_206 (55) = happyGoto action_89
action_206 (68) = happyGoto action_90
action_206 _ = happyFail

action_207 _ = happyReduce_138

action_208 _ = happyReduce_92

action_209 (127) = happyShift action_75
action_209 (128) = happyShift action_58
action_209 (45) = happyGoto action_210
action_209 (46) = happyGoto action_211
action_209 (47) = happyGoto action_212
action_209 (48) = happyGoto action_213
action_209 (58) = happyGoto action_214
action_209 (59) = happyGoto action_215
action_209 _ = happyFail

action_210 (103) = happyShift action_204
action_210 (119) = happyShift action_205
action_210 (63) = happyGoto action_237
action_210 (66) = happyGoto action_238
action_210 _ = happyFail

action_211 _ = happyReduce_77

action_212 (117) = happyShift action_236
action_212 (120) = happyShift action_207
action_212 (67) = happyGoto action_235
action_212 _ = happyFail

action_213 _ = happyReduce_80

action_214 _ = happyReduce_83

action_215 _ = happyReduce_82

action_216 (117) = happyShift action_123
action_216 _ = happyReduce_73

action_217 _ = happyReduce_72

action_218 (105) = happyShift action_199
action_218 (53) = happyGoto action_234
action_218 (54) = happyGoto action_198
action_218 _ = happyFail

action_219 _ = happyReduce_84

action_220 (71) = happyShift action_130
action_220 (72) = happyShift action_131
action_220 (73) = happyShift action_132
action_220 (74) = happyShift action_133
action_220 (75) = happyShift action_134
action_220 (77) = happyShift action_135
action_220 (78) = happyShift action_136
action_220 (80) = happyShift action_137
action_220 (81) = happyShift action_138
action_220 (82) = happyShift action_139
action_220 (89) = happyShift action_140
action_220 (90) = happyShift action_141
action_220 (91) = happyShift action_142
action_220 (92) = happyShift action_143
action_220 (93) = happyShift action_144
action_220 (94) = happyShift action_145
action_220 (95) = happyShift action_146
action_220 (96) = happyShift action_147
action_220 (97) = happyShift action_148
action_220 (98) = happyShift action_149
action_220 (99) = happyShift action_150
action_220 (100) = happyShift action_151
action_220 (101) = happyShift action_152
action_220 (102) = happyShift action_153
action_220 (106) = happyShift action_233
action_220 _ = happyFail

action_221 (87) = happyShift action_18
action_221 (104) = happyShift action_91
action_221 (107) = happyShift action_92
action_221 (108) = happyShift action_93
action_221 (109) = happyShift action_94
action_221 (113) = happyShift action_95
action_221 (114) = happyShift action_96
action_221 (121) = happyShift action_22
action_221 (124) = happyShift action_97
action_221 (125) = happyShift action_98
action_221 (131) = happyShift action_30
action_221 (132) = happyShift action_23
action_221 (15) = happyGoto action_78
action_221 (21) = happyGoto action_232
action_221 (22) = happyGoto action_80
action_221 (23) = happyGoto action_81
action_221 (24) = happyGoto action_82
action_221 (25) = happyGoto action_13
action_221 (26) = happyGoto action_14
action_221 (27) = happyGoto action_15
action_221 (28) = happyGoto action_16
action_221 (29) = happyGoto action_17
action_221 (32) = happyGoto action_83
action_221 (33) = happyGoto action_84
action_221 (34) = happyGoto action_62
action_221 (35) = happyGoto action_85
action_221 (39) = happyGoto action_86
action_221 (43) = happyGoto action_87
action_221 (49) = happyGoto action_88
action_221 (55) = happyGoto action_89
action_221 (68) = happyGoto action_90
action_221 _ = happyFail

action_222 _ = happyReduce_85

action_223 _ = happyReduce_136

action_224 _ = happyReduce_60

action_225 (76) = happyShift action_70
action_225 (79) = happyShift action_71
action_225 (90) = happyShift action_72
action_225 (121) = happyShift action_73
action_225 (126) = happyShift action_74
action_225 (127) = happyShift action_75
action_225 (128) = happyShift action_58
action_225 (129) = happyShift action_76
action_225 (130) = happyShift action_77
action_225 (131) = happyShift action_30
action_225 (132) = happyShift action_23
action_225 (15) = happyGoto action_60
action_225 (33) = happyGoto action_61
action_225 (34) = happyGoto action_62
action_225 (35) = happyGoto action_63
action_225 (38) = happyGoto action_231
action_225 (56) = happyGoto action_191
action_225 (57) = happyGoto action_65
action_225 (58) = happyGoto action_66
action_225 (59) = happyGoto action_67
action_225 (60) = happyGoto action_68
action_225 (61) = happyGoto action_69
action_225 _ = happyFail

action_226 _ = happyReduce_64

action_227 _ = happyReduce_54

action_228 _ = happyReduce_55

action_229 _ = happyReduce_56

action_230 _ = happyReduce_57

action_231 _ = happyReduce_68

action_232 (117) = happyShift action_123
action_232 _ = happyReduce_90

action_233 (76) = happyShift action_70
action_233 (79) = happyShift action_71
action_233 (90) = happyShift action_72
action_233 (121) = happyShift action_73
action_233 (126) = happyShift action_74
action_233 (127) = happyShift action_75
action_233 (128) = happyShift action_58
action_233 (129) = happyShift action_76
action_233 (130) = happyShift action_77
action_233 (131) = happyShift action_30
action_233 (132) = happyShift action_23
action_233 (15) = happyGoto action_60
action_233 (33) = happyGoto action_61
action_233 (34) = happyGoto action_62
action_233 (35) = happyGoto action_63
action_233 (56) = happyGoto action_242
action_233 (57) = happyGoto action_65
action_233 (58) = happyGoto action_66
action_233 (59) = happyGoto action_67
action_233 (60) = happyGoto action_68
action_233 (61) = happyGoto action_69
action_233 _ = happyFail

action_234 _ = happyReduce_89

action_235 (87) = happyShift action_18
action_235 (104) = happyShift action_91
action_235 (107) = happyShift action_92
action_235 (108) = happyShift action_93
action_235 (109) = happyShift action_94
action_235 (113) = happyShift action_95
action_235 (114) = happyShift action_96
action_235 (121) = happyShift action_22
action_235 (124) = happyShift action_97
action_235 (125) = happyShift action_98
action_235 (131) = happyShift action_30
action_235 (132) = happyShift action_23
action_235 (15) = happyGoto action_78
action_235 (21) = happyGoto action_241
action_235 (22) = happyGoto action_80
action_235 (23) = happyGoto action_81
action_235 (24) = happyGoto action_82
action_235 (25) = happyGoto action_13
action_235 (26) = happyGoto action_14
action_235 (27) = happyGoto action_15
action_235 (28) = happyGoto action_16
action_235 (29) = happyGoto action_17
action_235 (32) = happyGoto action_83
action_235 (33) = happyGoto action_84
action_235 (34) = happyGoto action_62
action_235 (35) = happyGoto action_85
action_235 (39) = happyGoto action_86
action_235 (43) = happyGoto action_87
action_235 (49) = happyGoto action_88
action_235 (55) = happyGoto action_89
action_235 (68) = happyGoto action_90
action_235 _ = happyFail

action_236 (127) = happyShift action_75
action_236 (128) = happyShift action_58
action_236 (48) = happyGoto action_240
action_236 (58) = happyGoto action_214
action_236 (59) = happyGoto action_215
action_236 _ = happyFail

action_237 (127) = happyShift action_75
action_237 (128) = happyShift action_58
action_237 (46) = happyGoto action_239
action_237 (47) = happyGoto action_212
action_237 (48) = happyGoto action_213
action_237 (58) = happyGoto action_214
action_237 (59) = happyGoto action_215
action_237 _ = happyFail

action_238 _ = happyReduce_75

action_239 _ = happyReduce_78

action_240 _ = happyReduce_81

action_241 (117) = happyShift action_123
action_241 _ = happyReduce_79

action_242 (71) = happyShift action_130
action_242 (72) = happyShift action_131
action_242 (73) = happyShift action_132
action_242 (74) = happyShift action_133
action_242 (75) = happyShift action_134
action_242 (77) = happyShift action_135
action_242 (78) = happyShift action_136
action_242 (80) = happyShift action_137
action_242 (81) = happyShift action_138
action_242 (82) = happyShift action_139
action_242 (89) = happyShift action_140
action_242 (90) = happyShift action_141
action_242 (91) = happyShift action_142
action_242 (92) = happyShift action_143
action_242 (93) = happyShift action_144
action_242 (94) = happyShift action_145
action_242 (95) = happyShift action_146
action_242 (96) = happyShift action_147
action_242 (97) = happyShift action_148
action_242 (98) = happyShift action_149
action_242 (99) = happyShift action_150
action_242 (100) = happyShift action_151
action_242 (101) = happyShift action_152
action_242 (102) = happyShift action_153
action_242 _ = happyReduce_91

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	_
	_
	 =  HappyAbsSyn4
		 (
	)

happyReduce_2 = happyMonadReduce 0 5 happyReduction_2
happyReduction_2 (happyRest) tk
	 = happyThen ((
        symbols %= openScope (Position (1,1)))
	) (\r -> happyReturn (HappyAbsSyn5 r))

happyReduce_3 = happyMonadReduce 0 6 happyReduction_3
happyReduction_3 (happyRest) tk
	 = happyThen ((
        symbols %= \st ->
            case goUp (closeScope EOFP st) of
                Left  _   -> st
                Right st' -> st')
	) (\r -> happyReturn (HappyAbsSyn6 r))

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn7
		 (
	)

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 _
	_
	 =  HappyAbsSyn7
		 (
	)

happyReduce_6 = happySpecReduce_2  8 happyReduction_6
happyReduction_6 _
	_
	 =  HappyAbsSyn8
		 (
	)

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 _
	_
	 =  HappyAbsSyn8
		 (
	)

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn8
		 (
	)

happyReduce_9 = happyMonadReduce 4 8 happyReduction_9
happyReduction_9 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( declStruct)
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyReduce_10 = happyMonadReduce 2 9 happyReduction_10
happyReduction_10 ((HappyAbsSyn15  happy_var_2) `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        current .= Just (item happy_var_2 :@ pos happy_var_1)
        curkind .= Just (item happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn9 r))

happyReduce_11 = happySpecReduce_1  10 happyReduction_11
happyReduction_11 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (EitherK :@ (pos happy_var_1)
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  10 happyReduction_12
happyReduction_12 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (RecordK :@ (pos happy_var_1)
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 _
	_
	_
	 =  HappyAbsSyn11
		 (
	)

happyReduce_14 = happyMonadReduce 5 12 happyReduction_14
happyReduction_14 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((
        current .= Just (item happy_var_2 :@ pos happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn12 r))

happyReduce_15 = happyMonadReduce 0 13 happyReduction_15
happyReduction_15 (happyRest) tk
	 = happyThen (( do
        storeProcedure voidT)
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_16 = happyMonadReduce 2 13 happyReduction_16
happyReduction_16 ((HappyAbsSyn25  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( do
        storeProcedure (item happy_var_2))
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_17 = happyMonadReduce 3 14 happyReduction_17
happyReduction_17 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( current .= Nothing)
	) (\r -> happyReturn (HappyAbsSyn14 r))

happyReduce_18 = happySpecReduce_1  15 happyReduction_18
happyReduction_18 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn15
		 (unTokenGenId `fmap` happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_0  16 happyReduction_19
happyReduction_19  =  HappyAbsSyn16
		 (
	)

happyReduce_20 = happySpecReduce_1  16 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn16
		 (
	)

happyReduce_21 = happySpecReduce_1  17 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn17
		 (
	)

happyReduce_22 = happySpecReduce_3  17 happyReduction_22
happyReduction_22 _
	_
	_
	 =  HappyAbsSyn17
		 (
	)

happyReduce_23 = happyMonadReduce 2 18 happyReduction_23
happyReduction_23 ((HappyAbsSyn34  happy_var_2) `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkDeclVar happy_var_1 happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn18 r))

happyReduce_24 = happySpecReduce_1  19 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn19
		 (
	)

happyReduce_25 = happySpecReduce_3  19 happyReduction_25
happyReduction_25 _
	_
	_
	 =  HappyAbsSyn19
		 (
	)

happyReduce_26 = happyMonadReduce 2 20 happyReduction_26
happyReduction_26 ((HappyAbsSyn34  happy_var_2) `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( verifyField happy_var_2 (item happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn20 r))

happyReduce_27 = happySpecReduce_1  21 happyReduction_27
happyReduction_27 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  21 happyReduction_28
happyReduction_28 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (checkBoth happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  22 happyReduction_29
happyReduction_29 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  22 happyReduction_30
happyReduction_30 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  22 happyReduction_31
happyReduction_31 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  22 happyReduction_32
happyReduction_32 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  22 happyReduction_33
happyReduction_33 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  22 happyReduction_34
happyReduction_34 (HappyAbsSyn43  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  22 happyReduction_35
happyReduction_35 (HappyAbsSyn49  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  22 happyReduction_36
happyReduction_36 (HappyAbsSyn55  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happyMonadReduce 1 22 happyReduction_37
happyReduction_37 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkAnswer voidT (pos happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn22 r))

happyReduce_38 = happyMonadReduce 2 22 happyReduction_38
happyReduction_38 ((HappyAbsSyn56  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkAnswer (item happy_var_2) (pos happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn22 r))

happyReduce_39 = happyMonadReduce 2 22 happyReduction_39
happyReduction_39 ((HappyAbsSyn56  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkWrite (item happy_var_2) (pos happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn22 r))

happyReduce_40 = happyMonadReduce 2 22 happyReduction_40
happyReduction_40 ((HappyAbsSyn33  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkRead (item happy_var_2) (pos happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn22 r))

happyReduce_41 = happyMonadReduce 2 23 happyReduction_41
happyReduction_41 ((HappyAbsSyn34  happy_var_2) `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do 
        ast %= (<|) (Declaration (pos happy_var_1) (item happy_var_1) (item happy_var_2) Nothing) 
        checkDeclVar happy_var_1 happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn23 r))

happyReduce_42 = happyMonadReduce 4 24 happyReduction_42
happyReduction_42 ((HappyAbsSyn56  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn34  happy_var_2) `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkInit happy_var_1 happy_var_2 happy_var_4)
	) (\r -> happyReturn (HappyAbsSyn24 r))

happyReduce_43 = happyMonadReduce 1 25 happyReduction_43
happyReduction_43 ((HappyAbsSyn26  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( case happy_var_1 of
        (Undef tname :@ p) -> do
            err $ UndefinedType tname p
            return (None :@ p)
        _ -> return happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn25 r))

happyReduce_44 = happySpecReduce_2  26 happyReduction_44
happyReduction_44 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn26
		 (arrayPadding $ buildArray happy_var_2 `fmap` happy_var_1
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  27 happyReduction_45
happyReduction_45 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  27 happyReduction_46
happyReduction_46 (HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn27
		 (buildPointers (item happy_var_1) (item happy_var_2) :@ (pos happy_var_1)
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happyMonadReduce 1 28 happyReduction_47
happyReduction_47 ((HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        t <- findType (item happy_var_1)
        return (t :@ pos happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn28 r))

happyReduce_48 = happySpecReduce_3  28 happyReduction_48
happyReduction_48 _
	(HappyAbsSyn26  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn28
		 ((item happy_var_2) :@ (pos happy_var_1)
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  29 happyReduction_49
happyReduction_49 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn29
		 (1 :@ (pos happy_var_1)
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  29 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn29
		 ((+1) `fmap` happy_var_1
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_0  30 happyReduction_51
happyReduction_51  =  HappyAbsSyn30
		 (Seq.empty
	)

happyReduce_52 = happySpecReduce_2  30 happyReduction_52
happyReduction_52 (HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1 |> happy_var_2
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  31 happyReduction_53
happyReduction_53 _
	(HappyAbsSyn59  happy_var_2)
	_
	 =  HappyAbsSyn31
		 ((      0    , item happy_var_2 - 1)
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happyReduce 5 31 happyReduction_54
happyReduction_54 (_ `HappyStk`
	(HappyAbsSyn59  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn59  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn31
		 ((item happy_var_2    , item happy_var_4    )
	) `HappyStk` happyRest

happyReduce_55 = happyReduce 5 31 happyReduction_55
happyReduction_55 (_ `HappyStk`
	(HappyAbsSyn59  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn59  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn31
		 ((item happy_var_2    , item happy_var_4 - 1)
	) `HappyStk` happyRest

happyReduce_56 = happyReduce 5 31 happyReduction_56
happyReduction_56 (_ `HappyStk`
	(HappyAbsSyn59  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn59  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn31
		 ((item happy_var_2 + 1, item happy_var_4    )
	) `HappyStk` happyRest

happyReduce_57 = happyReduce 5 31 happyReduction_57
happyReduction_57 (_ `HappyStk`
	(HappyAbsSyn59  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn59  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn31
		 ((item happy_var_2 + 1, item happy_var_4 - 1)
	) `HappyStk` happyRest

happyReduce_58 = happyMonadReduce 3 32 happyReduction_58
happyReduction_58 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn33  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        Assign (pos happy_var_1) (item happy_var_1) (item happy_var_2) 
        checkAssign happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn32 r))

happyReduce_59 = happyMonadReduce 1 33 happyReduction_59
happyReduction_59 ((HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen ((do
        isSymbol' happy_var_1
        findTypeOfSymbol happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn33 r))

happyReduce_60 = happyMonadReduce 4 33 happyReduction_60
happyReduction_60 (_ `HappyStk`
	(HappyAbsSyn56  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkArray happy_var_1 (item happy_var_3))
	) (\r -> happyReturn (HappyAbsSyn33 r))

happyReduce_61 = happyMonadReduce 3 33 happyReduction_61
happyReduction_61 ((HappyAbsSyn34  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( happy_var_1 `getField` happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn33 r))

happyReduce_62 = happyMonadReduce 2 33 happyReduction_62
happyReduction_62 (_ `HappyStk`
	(HappyAbsSyn33  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( deref happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn33 r))

happyReduce_63 = happySpecReduce_1  34 happyReduction_63
happyReduction_63 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn34
		 (unTokenVarId `fmap` happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happyMonadReduce 4 35 happyReduction_64
happyReduction_64 (_ `HappyStk`
	(HappyAbsSyn36  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkCall happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn35 r))

happyReduce_65 = happySpecReduce_0  36 happyReduction_65
happyReduction_65  =  HappyAbsSyn36
		 (Seq.empty
	)

happyReduce_66 = happySpecReduce_1  36 happyReduction_66
happyReduction_66 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  37 happyReduction_67
happyReduction_67 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn37
		 (Seq.singleton happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  37 happyReduction_68
happyReduction_68 (HappyAbsSyn38  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn37
		 (happy_var_1 |> happy_var_3
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  38 happyReduction_69
happyReduction_69 (HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn38
		 (item happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_3  39 happyReduction_70
happyReduction_70 _
	(HappyAbsSyn40  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn39
		 (item happy_var_2 :@ pos happy_var_1
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  40 happyReduction_71
happyReduction_71 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_3  40 happyReduction_72
happyReduction_72 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn40
		 (checkBoth happy_var_1 happy_var_3
	)
happyReduction_72 _ _ _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  41 happyReduction_73
happyReduction_73 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn42  happy_var_1)
	 =  HappyAbsSyn41
		 (checkBoth happy_var_1 happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happyMonadReduce 1 42 happyReduction_74
happyReduction_74 ((HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        if item happy_var_1 == boolT
            then return $ voidT :@ pos happy_var_1
            else do
                err $ InvalidGuard (item happy_var_1) (pos happy_var_1)
                return $ None :@ pos happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn42 r))

happyReduce_75 = happyMonadReduce 5 43 happyReduction_75
happyReduction_75 (_ `HappyStk`
	(HappyAbsSyn45  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn44  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( do
        caseTypes %= tail
        return $ checkBoth happy_var_2 happy_var_4)
	) (\r -> happyReturn (HappyAbsSyn43 r))

happyReduce_76 = happyMonadReduce 1 44 happyReduction_76
happyReduction_76 ((HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( if (item happy_var_1) `elem` [intT, charT]
        then do
            caseTypes %= (happy_var_1 :)
            return happy_var_1
        else do
            caseTypes %= ((None :@ pos happy_var_1) :)
            err $ BadCaseExp (item happy_var_1) (pos happy_var_1)
            return $ None :@ pos happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn44 r))

happyReduce_77 = happySpecReduce_1  45 happyReduction_77
happyReduction_77 (HappyAbsSyn46  happy_var_1)
	 =  HappyAbsSyn45
		 (happy_var_1
	)
happyReduction_77 _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  45 happyReduction_78
happyReduction_78 (HappyAbsSyn46  happy_var_3)
	_
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn45
		 (checkBoth happy_var_1 happy_var_3
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  46 happyReduction_79
happyReduction_79 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn46
		 (checkBoth happy_var_1 happy_var_3
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  47 happyReduction_80
happyReduction_80 (HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn47
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  47 happyReduction_81
happyReduction_81 (HappyAbsSyn48  happy_var_3)
	_
	(HappyAbsSyn47  happy_var_1)
	 =  HappyAbsSyn47
		 (checkBoth happy_var_1 happy_var_3
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happyMonadReduce 1 48 happyReduction_82
happyReduction_82 ((HappyAbsSyn59  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        ((ct :@ p):_) <- use caseTypes
        if (ct == intT)
            then return $ intT :@ pos happy_var_1
            else do
                err $ BadCaseCharElem p (item happy_var_1) (pos happy_var_1)
                return $ None :@ (pos happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn48 r))

happyReduce_83 = happyMonadReduce 1 48 happyReduction_83
happyReduction_83 ((HappyAbsSyn58  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        ((ct :@ p):_) <- use caseTypes
        if (ct == charT)
            then return $ charT :@ pos happy_var_1
            else do
                err $ BadCaseIntElem p (item happy_var_1) (pos happy_var_1)
                return $ None :@ (pos happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn48 r))

happyReduce_84 = happyMonadReduce 4 49 happyReduction_84
happyReduction_84 (_ `HappyStk`
	(HappyAbsSyn52  happy_var_3) `HappyStk`
	(HappyAbsSyn51  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( do
        forVars %= tail
        return $ checkBoth happy_var_2 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn49 r))

happyReduce_85 = happyMonadReduce 4 49 happyReduction_85
happyReduction_85 (_ `HappyStk`
	(HappyAbsSyn52  happy_var_3) `HappyStk`
	(HappyAbsSyn50  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( do
        forVars %= tail
        return $ checkBoth happy_var_2 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn49 r))

happyReduce_86 = happyMonadReduce 2 50 happyReduction_86
happyReduction_86 ((HappyAbsSyn34  happy_var_2) `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        if (item happy_var_1) `elem` [intT, charT]
            then do
                t :@ p <- checkDeclVar happy_var_1 happy_var_2
                case t of
                    None -> do
                        forVars %= ((happy_var_2, None):)
                        return $ None :@ p
                    _    -> do
                        forVars %= ((happy_var_2, item happy_var_1):)
                        return $ voidT :@ p
            else do
                checkDeclVar (None :@ (pos happy_var_1)) happy_var_2
                err $ BadForVar (item happy_var_2) (item happy_var_1) (pos happy_var_1) (pos happy_var_1)
                forVars %= ((happy_var_2, None):)
                return $ None :@ (pos happy_var_1))
	) (\r -> happyReturn (HappyAbsSyn50 r))

happyReduce_87 = happyMonadReduce 1 51 happyReduction_87
happyReduction_87 ((HappyAbsSyn34  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        (t :@ p) <- findTypeOfSymbol happy_var_1
        if (t `elem` [intT, charT])
            then do
                forVars %= ((happy_var_1, t):)
                return $ voidT :@ p
            else do
                err $ BadForVar (item happy_var_1) t p (pos happy_var_1)
                forVars %= ((happy_var_1, None):)
                return $ None :@ p)
	) (\r -> happyReturn (HappyAbsSyn51 r))

happyReduce_88 = happySpecReduce_1  52 happyReduction_88
happyReduction_88 (HappyAbsSyn53  happy_var_1)
	 =  HappyAbsSyn52
		 (happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_3  52 happyReduction_89
happyReduction_89 (HappyAbsSyn53  happy_var_3)
	_
	(HappyAbsSyn52  happy_var_1)
	 =  HappyAbsSyn52
		 (checkBoth happy_var_1 happy_var_3
	)
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_3  53 happyReduction_90
happyReduction_90 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn54  happy_var_1)
	 =  HappyAbsSyn53
		 (checkBoth happy_var_1 happy_var_3
	)
happyReduction_90 _ _ _  = notHappyAtAll 

happyReduce_91 = happyMonadReduce 4 54 happyReduction_91
happyReduction_91 ((HappyAbsSyn56  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn56  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen (( checkFor happy_var_2 happy_var_4)
	) (\r -> happyReturn (HappyAbsSyn54 r))

happyReduce_92 = happySpecReduce_3  55 happyReduction_92
happyReduction_92 _
	(HappyAbsSyn40  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn55
		 (item happy_var_2 :@ pos happy_var_1
	)
happyReduction_92 _ _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_3  56 happyReduction_93
happyReduction_93 _
	(HappyAbsSyn56  happy_var_2)
	_
	 =  HappyAbsSyn56
		 (happy_var_2
	)
happyReduction_93 _ _ _  = notHappyAtAll 

happyReduce_94 = happyMonadReduce 1 56 happyReduction_94
happyReduction_94 ((HappyAbsSyn57  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do 
                                        expression (:happy_var_1) 
                                        boolT   :@ pos happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_95 = happyMonadReduce 1 56 happyReduction_95
happyReduction_95 ((HappyAbsSyn58  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do 
                                        expression (:happy_var_1)  
                                        charT   :@ pos happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_96 = happyMonadReduce 1 56 happyReduction_96
happyReduction_96 ((HappyAbsSyn59  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do 
                                        expression (:happy_var_1) 
                                        intT    :@ pos happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_97 = happyMonadReduce 1 56 happyReduction_97
happyReduction_97 ((HappyAbsSyn60  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do 
                                        expression (:happy_var_1) 
                                        floatT  :@ pos happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_98 = happyMonadReduce 1 56 happyReduction_98
happyReduction_98 ((HappyAbsSyn61  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do 
                                        expression (:happy_var_1) 
                                        stringT :@ pos happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_99 = happySpecReduce_1  56 happyReduction_99
happyReduction_99 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn56
		 (happy_var_1
	)
happyReduction_99 _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_1  56 happyReduction_100
happyReduction_100 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn56
		 (happy_var_1
	)
happyReduction_100 _  = notHappyAtAll 

happyReduce_101 = happyMonadReduce 3 56 happyReduction_101
happyReduction_101 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp And (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_102 = happyMonadReduce 3 56 happyReduction_102
happyReduction_102 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp Andalso (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_103 = happyMonadReduce 3 56 happyReduction_103
happyReduction_103 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp (Or :@ (pos 2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_104 = happyMonadReduce 3 56 happyReduction_104
happyReduction_104 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp Orelse (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_105 = happyMonadReduce 3 56 happyReduction_105
happyReduction_105 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp Xor (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_106 = happyMonadReduce 2 56 happyReduction_106
happyReduction_106 ((HappyAbsSyn56  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkUnOp  Not      (pos happy_var_1) happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_107 = happyMonadReduce 3 56 happyReduction_107
happyReduction_107 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp Band (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_108 = happyMonadReduce 3 56 happyReduction_108
happyReduction_108 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp Bor (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_109 = happyMonadReduce 3 56 happyReduction_109
happyReduction_109 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp Bsl (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_110 = happyMonadReduce 3 56 happyReduction_110
happyReduction_110 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp Bsr (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_111 = happyMonadReduce 3 56 happyReduction_111
happyReduction_111 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp Bxor (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_112 = happyMonadReduce 2 56 happyReduction_112
happyReduction_112 ((HappyAbsSyn56  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkUnOp  Bnot (pos happy_var_1) happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_113 = happyMonadReduce 3 56 happyReduction_113
happyReduction_113 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp Plus (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_114 = happyMonadReduce 3 56 happyReduction_114
happyReduction_114 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp Minus (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_115 = happyMonadReduce 3 56 happyReduction_115
happyReduction_115 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp Times (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_116 = happyMonadReduce 3 56 happyReduction_116
happyReduction_116 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp FloatDiv (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_117 = happyMonadReduce 3 56 happyReduction_117
happyReduction_117 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp IntDiv (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_118 = happyMonadReduce 3 56 happyReduction_118
happyReduction_118 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp Rem (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_119 = happyMonadReduce 2 56 happyReduction_119
happyReduction_119 ((HappyAbsSyn56  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkUnOp  Uminus   (pos happy_var_1) happy_var_2)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_120 = happyMonadReduce 3 56 happyReduction_120
happyReduction_120 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp LTop (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_121 = happyMonadReduce 3 56 happyReduction_121
happyReduction_121 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp LEop (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_122 = happyMonadReduce 3 56 happyReduction_122
happyReduction_122 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp GTop (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_123 = happyMonadReduce 3 56 happyReduction_123
happyReduction_123 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp GEop (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_124 = happyMonadReduce 3 56 happyReduction_124
happyReduction_124 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp EQop (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_125 = happyMonadReduce 3 56 happyReduction_125
happyReduction_125 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp NEop (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_126 = happyMonadReduce 3 56 happyReduction_126
happyReduction_126 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp FAop (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_127 = happyMonadReduce 3 56 happyReduction_127
happyReduction_127 ((HappyAbsSyn56  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn56  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( checkBinOp NFop (pos happy_var_2) happy_var_1 happy_var_3)
	) (\r -> happyReturn (HappyAbsSyn56 r))

happyReduce_128 = happySpecReduce_1  57 happyReduction_128
happyReduction_128 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn57
		 (unTokenBoolLit `fmap` happy_var_1
	)
happyReduction_128 _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_1  58 happyReduction_129
happyReduction_129 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn58
		 (unTokenCharLit `fmap` happy_var_1
	)
happyReduction_129 _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_1  59 happyReduction_130
happyReduction_130 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn59
		 (unTokenIntLit  `fmap` happy_var_1
	)
happyReduction_130 _  = notHappyAtAll 

happyReduce_131 = happySpecReduce_1  60 happyReduction_131
happyReduction_131 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn60
		 (unTokenFloatLit `fmap` happy_var_1
	)
happyReduction_131 _  = notHappyAtAll 

happyReduce_132 = happyMonadReduce 1 61 happyReduction_132
happyReduction_132 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        string happy_var_1
        return $ unTokenStringLit `fmap` happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn61 r))

happyReduce_133 = happyMonadReduce 1 62 happyReduction_133
happyReduction_133 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        symbols %= \st ->
            case goUp (closeScope (pos happy_var_1) st) of
                Left  _   -> st
                Right st' -> st'
        offset %= (\(x:xs) -> xs)
        return happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn62 r))

happyReduce_134 = happyMonadReduce 1 63 happyReduction_134
happyReduction_134 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        symbols %= \st ->
            case goUp (closeScope (pos happy_var_1) st) of
                Left  _   -> st
                Right st' -> st'
        offset %= (\(x:xs) -> xs)
        return happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn63 r))

happyReduce_135 = happyMonadReduce 1 64 happyReduction_135
happyReduction_135 ((HappyAbsSyn62  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        symbols %= \st ->
            case goUp (closeScope (pos happy_var_1) st) of
                Left  _   -> st
                Right st' -> st'
        offset %= (\(x:xs) -> xs)
        return happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn64 r))

happyReduce_136 = happyMonadReduce 1 65 happyReduction_136
happyReduction_136 ((HappyAbsSyn66  happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        symbols %= \st ->
            case goUp (closeScope (pos happy_var_1) st) of
                Left  _   -> st
                Right st' -> st'
        offset %= (\(x:xs) -> xs)
        return happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn65 r))

happyReduce_137 = happyMonadReduce 1 66 happyReduction_137
happyReduction_137 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        symbols %= \st ->
            case goUp (closeScope (pos happy_var_1) st) of
                Left  _   -> st
                Right st' -> st'
        offset %= (\(x:xs) -> xs)
        return happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn66 r))

happyReduce_138 = happyMonadReduce 1 67 happyReduction_138
happyReduction_138 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        symbols %= openScope (pos happy_var_1)
        offset %= (\(x:xs) -> (x:x:xs))
        return happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn67 r))

happyReduce_139 = happyMonadReduce 1 68 happyReduction_139
happyReduction_139 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        symbols %= openScope (pos happy_var_1)
        offset %= (\(x:xs) -> (x:x:xs))
        return happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn68 r))

happyReduce_140 = happyMonadReduce 1 69 happyReduction_140
happyReduction_140 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        symbols %= openScope (pos happy_var_1)
        offset %= (0:)
        return happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn69 r))

happyReduce_141 = happyMonadReduce 1 70 happyReduction_141
happyReduction_141 ((HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( do
        symbols %= openScope (pos happy_var_1)
        offset %= (0:)
        return happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn70 r))

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	EOF :@ _ -> action 133 133 tk (HappyState action) sts stk;
	TokenAnd     :@ _ -> cont 71;
	TokenAndalso :@ _ -> cont 72;
	TokenOr      :@ _ -> cont 73;
	TokenOrelse  :@ _ -> cont 74;
	TokenXor     :@ _ -> cont 75;
	TokenNot     :@ _ -> cont 76;
	TokenBand :@ _ -> cont 77;
	TokenBor  :@ _ -> cont 78;
	TokenBnot :@ _ -> cont 79;
	TokenBxor :@ _ -> cont 80;
	TokenBxor :@ _ -> cont 81;
	TokenBxor :@ _ -> cont 82;
	TokenLeftBracket  :@ _ -> cont 83;
	TokenRightBracket :@ _ -> cont 84;
	TokenLeftBrace    :@ _ -> cont 85;
	TokenRightBrace   :@ _ -> cont 86;
	TokenCaret        :@ _ -> cont 87;
	TokenUnderscore   :@ _ -> cont 88;
	TokenPlus     :@ _ -> cont 89;
	TokenMinus    :@ _ -> cont 90;
	TokenTimes    :@ _ -> cont 91;
	TokenFloatDiv :@ _ -> cont 92;
	TokenIntDiv   :@ _ -> cont 93;
	TokenRem      :@ _ -> cont 94;
	TokenLT :@ _ -> cont 95;
	TokenLE :@ _ -> cont 96;
	TokenGT :@ _ -> cont 97;
	TokenGE :@ _ -> cont 98;
	TokenEQ :@ _ -> cont 99;
	TokenNE :@ _ -> cont 100;
	TokenFA :@ _ -> cont 101;
	TokenNF :@ _ -> cont 102;
	TokenEnd       :@ _ -> cont 103;
	TokenFor       :@ _ -> cont 104;
	TokenFrom      :@ _ -> cont 105;
	TokenTo        :@ _ -> cont 106;
	TokenIf        :@ _ -> cont 107;
	TokenWhile     :@ _ -> cont 108;
	TokenCase      :@ _ -> cont 109;
	TokenOf        :@ _ -> cont 110;
	TokenProcedure :@ _ -> cont 111;
	TokenDefine    :@ _ -> cont 112;
	TokenFinish    :@ _ -> cont 113;
	TokenAnswer    :@ _ -> cont 114;
	TokenEither :@ _ -> cont 115;
	TokenRecord :@ _ -> cont 116;
	TokenComma     :@ _ -> cont 117;
	TokenPeriod    :@ _ -> cont 118;
	TokenSemicolon :@ _ -> cont 119;
	TokenArrow     :@ _ -> cont 120;
	TokenLeftPar   :@ _ -> cont 121;
	TokenRightPar  :@ _ -> cont 122;
	TokenIs :@ _ -> cont 123;
	TokenRead  :@ _ -> cont 124;
	TokenWrite :@ _ -> cont 125;
	TokenBoolLit   _ :@ _ -> cont 126;
	TokenCharLit   _ :@ _ -> cont 127;
	TokenIntLit    _ :@ _ -> cont 128;
	TokenFloatLit  _ :@ _ -> cont 129;
	TokenStringLit _ :@ _ -> cont 130;
	TokenVarId _ :@ _ -> cont 131;
	TokenGenId _ :@ _ -> cont 132;
	_ -> happyError' tk
	})

happyError_ 133 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => Epilog a -> (a -> Epilog b) -> Epilog b
happyThen = (>>=)
happyReturn :: () => a -> Epilog a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> Epilog a
happyReturn1 = happyReturn
happyError' :: () => (At Token) -> Epilog a
happyError' tk = parseError tk

parse = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


------------------------------------------------------------------------------
parseError :: At Token -> Epilog a
parseError (t :@ p) = do
    err $ UnexpectedToken t p
    return undefined
{-# LINE 1 "templates/GenericTemplate.hs" #-}


















-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

























infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

