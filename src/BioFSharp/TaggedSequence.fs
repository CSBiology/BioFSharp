namespace BioFSharp

open System

module TaggedSequence =

    /// record of a sequence and its tag
    type TaggedSequence<'a,'b> ={
        Tag: 'a;
        Sequence: seq<'b>}

    /// Creates a tagged sequence
    let createTaggedSequence tag sequence = 
        {Tag = tag; Sequence = sequence}

    /// Maps tag of tagged sequence
    let mapTag (mapping:'a->'c) (ts:TaggedSequence<'a,'b>) =
        {Tag = mapping ts.Tag; Sequence = ts.Sequence}

    /// Maps sequence of tagged sequence
    let mapSequence (mapping:seq<'b>->seq<'c>) (ts:TaggedSequence<'a,'b>) =
        {Tag = ts.Tag; Sequence = mapping ts.Sequence}