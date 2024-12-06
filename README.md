Fig1とFig3の出現ノードの統一をした  
その際、igraphで描画する際にedge.arrow.size と edge.arrow.width を同時に指定するとヤジリの色などがおかしくなるバグを発見した  
このバグを回避するため、Fig1abとFig3のスクリプトには引数に差異を作らざるを得なかった（が、本質的には同等のものを描画した）  
・Fig1 edge.arrow.sizeを採用（edge.arrow.widthだとFig1(a)でerror発生）  
・Fig3 edge.arrow.widthを採用（edge.arrow.sizeだとヤジリと線の色があべこべになる  
