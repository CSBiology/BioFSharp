namespace BioFSharp

open System

/// Record of a sequence and its tag
type TaggedSequence<'T,'S> =
    {
        Tag: 'T;
        Sequence: seq<'S>
    }
        with

        /// Creates a tagged sequence
        static member create (tag:'T) (sequence:seq<'S>) = 
            {Tag = tag; Sequence = sequence}

        /// Maps tag of tagged sequence
        static member mapTag (mapping:'T->'U) (ts:TaggedSequence<'T,'S>) : TaggedSequence<'U,'S> =
            TaggedSequence.create
                (mapping ts.Tag)
                ts.Sequence

        /// Maps sequence of tagged sequence
        static member mapSequence (mapping:seq<'S>->seq<'M>) (ts:TaggedSequence<'T,'S>) =
            TaggedSequence.create
                ts.Tag
                (mapping ts.Sequence)

