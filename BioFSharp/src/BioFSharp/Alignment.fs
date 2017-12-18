namespace BioFSharp

///Basic structure and functionality for sequence alignments. Alignment functions can be found in the BioFSharp.Algorithm namespace.
module Alignment = 
        open BioID.FastA
        
        ///General Alignment type used throughout BioFSharp
        type Alignment<'Sequence,'Metadata> =                
                {
                ///Additional information for this alignment
                MetaData            : 'Metadata;
                ///List of aligned Sequences
                AlignedSequences    : list<'Sequence>;
                }
        
        let mapSequences (mapping: 'Sequence -> 'mSequence) (alignment: Alignment<'Sequence,_>) =       
            {MetaData = alignment.MetaData;
            AlignedSequences = List.map (mapping) alignment.AlignedSequences}
    