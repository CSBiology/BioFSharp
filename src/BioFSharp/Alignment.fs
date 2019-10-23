namespace BioFSharp

///Basic structure and functionality for sequence alignments. Alignment functions can be found in the BioFSharp.Algorithm namespace.
module Alignment = 
        
        
        ///General Alignment type used throughout BioFSharp
        type Alignment<'Sequence,'Metadata> =                
                {
                ///Additional information for this alignment
                MetaData            : 'Metadata;
                ///List of aligned Sequences
                Sequences    : seq<'Sequence>;
                }
        
        ///Creates Alignment type 
        let createAlignment metaData sequences =
            {MetaData=metaData;Sequences=sequences}

        ///Mapping function for aligned sequences, keeping the associated metadata.
        let mapSequences (mapping: 'Sequence -> 'mSequence) (alignment: Alignment<'Sequence,_>) =       
            {MetaData = alignment.MetaData; Sequences = Seq.map (mapping) alignment.Sequences}


        ///Mapping function for aligned sequences, keeping the associated metadata.
        let mapmetaData (mapping: 'Metadata -> 'NewMetadata) (alignment: Alignment<'Sequence,_>) =       
            { MetaData = mapping alignment.MetaData; Sequences = alignment.Sequences}    

