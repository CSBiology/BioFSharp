namespace BioFSharp

///Basic structure and functionality for sequence alignments. Alignment functions can be found in the BioFSharp.Algorithm namespace.
module Alignment = 
        
        ///General Alignment type used throughout BioFSharp
        type Alignment<'Sequence,'Metadata> =                
                {
                ///Additional information for this alignment
                MetaData            : 'Metadata;
                ///List of aligned Sequences
                AlignedSequences    : list<'Sequence>;
                }

