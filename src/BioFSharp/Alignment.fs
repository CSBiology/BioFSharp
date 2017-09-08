namespace BioFSharp

///Basic structure and functionality for sequence alignments. Functions for aligning functions can be found in the BioFSharp.Algorithm namespace.
module Alignment = 
        
        type Alignment<'Sequence,'Metadata> = 
                
                {
                MetaData            : 'Metadata;
                AlignedSequences    : list<'Sequence>;
                }

