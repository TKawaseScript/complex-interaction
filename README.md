Fig1とFig3の出現ノードの統一しました  
Fig1とFig3のスクリプトに関して  
  igraphで描画する際にバグを回避するため引数に差異があることに注意  
    バグ: edge.arrow.size と edge.arrow.width を同時に指定するとヤジリの色などがおかしくなる  
    Fig1 edge.arrow.sizeを採用  
      edge.arrow.widthだとFig1(a)でerror
    Fig3 edge.arrow.widthを採用  
      edge.arrow.sizeだとヤジリと線の色があべこべになる
