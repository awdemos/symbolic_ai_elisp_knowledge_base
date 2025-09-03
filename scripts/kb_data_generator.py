#!/usr/bin/env python3
"""
Knowledge Base Data Generator using Outlines

This script uses the Outlines library to generate structured knowledge base facts
that can be imported into the Emacs Lisp knowledge base system.

Usage:
    python kb_data_generator.py --text "Some text to extract facts from" --output facts.el
    python kb_data_generator.py --file input.txt --output facts.el --microtheory MyMt
"""

import argparse
import json
import sys
from typing import List, Optional, Union, Literal
from datetime import datetime
from pathlib import Path

try:
    import outlines
    from pydantic import BaseModel, Field
except ImportError:
    print("Error: Required packages not installed. Install with:")
    print("pip install outlines[transformers] pydantic")
    sys.exit(1)

# Data models for structured fact extraction
class KnowledgeFact(BaseModel):
    """Represents a knowledge base fact in subject-predicate-object format."""
    subject: str = Field(description="The subject entity of the fact")
    predicate: str = Field(description="The relationship or property")
    object: Union[str, int, float, bool] = Field(description="The object/value of the fact")
    certainty: float = Field(default=1.0, ge=0.0, le=1.0, description="Confidence level")
    microtheory: Optional[str] = Field(default=None, description="Context/microtheory for the fact")

class TemporalFact(BaseModel):
    """Represents a temporally bounded fact."""
    subject: str
    predicate: str
    object: Union[str, int, float, bool]
    valid_from: str = Field(description="Start time in ISO format")
    valid_to: str = Field(description="End time in ISO format")
    certainty: float = Field(default=1.0, ge=0.0, le=1.0)
    microtheory: Optional[str] = Field(default=None)

class Event(BaseModel):
    """Represents an event in the knowledge base."""
    event_type: str
    participants: List[str]
    location: Optional[str] = None
    time: Optional[str] = None
    properties: Optional[dict] = None

class ExtractionResult(BaseModel):
    """Complete result of fact extraction from text."""
    facts: List[KnowledgeFact]
    temporal_facts: List[TemporalFact] = Field(default_factory=list)
    events: List[Event] = Field(default_factory=list)

class KBDataGenerator:
    """Generates structured KB data using Outlines."""
    
    def __init__(self, model_name: str = "microsoft/Phi-3-mini-4k-instruct"):
        """Initialize with a specific model."""
        self.model_name = model_name
        self.model = None
        self._init_model()
    
    def _init_model(self):
        """Initialize the Outlines model."""
        try:
            # Try to use transformers first
            from transformers import AutoTokenizer, AutoModelForCausalLM
            
            tokenizer = AutoTokenizer.from_pretrained(self.model_name)
            model = AutoModelForCausalLM.from_pretrained(
                self.model_name, 
                device_map="auto",
                torch_dtype="auto"
            )
            
            self.model = outlines.from_transformers(model, tokenizer)
            print(f"Initialized model: {self.model_name}")
            
        except Exception as e:
            print(f"Warning: Could not initialize transformers model: {e}")
            print("You may need to install transformers: pip install transformers torch")
            # Fallback to mock for demonstration
            self.model = None
    
    def extract_facts(self, text: str, microtheory: Optional[str] = None) -> ExtractionResult:
        """Extract structured facts from text using the LLM."""
        if not self.model:
            # Return mock data for demonstration
            return self._generate_mock_facts(text, microtheory)
        
        prompt = f"""
Extract knowledge base facts from the following text. Focus on:
- Basic relationships (is-a, has, located-at, etc.)
- Properties and attributes
- Temporal information if present
- Events and their participants

Text:
{text}

Extract facts as subject-predicate-object triplets with appropriate confidence levels.
For temporal facts, include start and end times in ISO format.
For events, identify the event type, participants, location, and time.
"""
        
        try:
            result = self.model(prompt, ExtractionResult, max_new_tokens=1000)
            
            # Set microtheory if specified
            if microtheory:
                for fact in result.facts:
                    if not fact.microtheory:
                        fact.microtheory = microtheory
                for fact in result.temporal_facts:
                    if not fact.microtheory:
                        fact.microtheory = microtheory
            
            return result
        
        except Exception as e:
            print(f"Error during fact extraction: {e}")
            return self._generate_mock_facts(text, microtheory)
    
    def _generate_mock_facts(self, text: str, microtheory: Optional[str]) -> ExtractionResult:
        """Generate mock facts for demonstration when model is unavailable."""
        # Simple pattern-based extraction for demonstration
        facts = []
        
        # Basic pattern matching (this would be replaced by LLM in real usage)
        if "Einstein" in text:
            facts.extend([
                KnowledgeFact(subject="Einstein", predicate="is-a", object="physicist", 
                             certainty=0.95, microtheory=microtheory),
                KnowledgeFact(subject="Einstein", predicate="developed", object="relativity_theory", 
                             certainty=0.9, microtheory=microtheory)
            ])
        
        if "Socrates" in text:
            facts.extend([
                KnowledgeFact(subject="Socrates", predicate="is-a", object="philosopher", 
                             certainty=0.95, microtheory=microtheory),
                KnowledgeFact(subject="Socrates", predicate="born-in", object="Athens", 
                             certainty=0.8, microtheory=microtheory)
            ])
        
        return ExtractionResult(facts=facts)

def format_elisp_fact(fact: KnowledgeFact) -> str:
    """Convert a KnowledgeFact to Elisp kb-assert format."""
    # Format object based on type
    if isinstance(fact.object, str):
        if fact.object.replace("_", "").replace("-", "").isalnum():
            obj_str = f"'{fact.object}"
        else:
            obj_str = f'"{fact.object}"'
    elif isinstance(fact.object, bool):
        obj_str = "t" if fact.object else "nil"
    else:
        obj_str = str(fact.object)
    
    # Build the assertion
    assertion = f"(kb-assert '{fact.subject} '{fact.predicate} {obj_str}"
    
    # Add certainty if not 1.0
    if fact.certainty != 1.0:
        assertion += f" {fact.certainty}"
    
    # Add temporal-info placeholder if needed
    if fact.certainty != 1.0:
        assertion += " nil"  # temporal-info
    
    # Add microtheory if specified
    if fact.microtheory:
        if fact.certainty == 1.0:
            assertion += f" nil nil '{fact.microtheory}"
        else:
            assertion += f" '{fact.microtheory}"
    
    assertion += ")"
    return assertion

def format_elisp_temporal_fact(fact: TemporalFact) -> str:
    """Convert a TemporalFact to Elisp kb-assert-temporal format."""
    obj_str = f"'{fact.object}" if isinstance(fact.object, str) else str(fact.object)
    
    assertion = f"(kb-assert-temporal '{fact.subject} '{fact.predicate} {obj_str}"
    assertion += f' "{fact.valid_from}" "{fact.valid_to}"'
    
    if fact.certainty != 1.0:
        assertion += f" {fact.certainty}"
    
    if fact.microtheory:
        assertion += f" '{fact.microtheory}"
    
    assertion += ")"
    return assertion

def format_elisp_event(event: Event) -> str:
    """Convert an Event to Elisp kb-create-event format."""
    participants_str = "(list " + " ".join(f"'{p}" for p in event.participants) + ")"
    
    assertion = f"(kb-create-event '{event.event_type}"
    assertion += f" :participants {participants_str}"
    
    if event.location:
        assertion += f" :location '{event.location}"
    
    if event.time:
        assertion += f' :time "{event.time}"'
    
    if event.properties:
        for key, value in event.properties.items():
            if isinstance(value, str):
                assertion += f' :{key} "{value}"'
            else:
                assertion += f" :{key} {value}"
    
    assertion += ")"
    return assertion

def generate_elisp_file(extraction_result: ExtractionResult, 
                       output_file: str, 
                       microtheory: Optional[str] = None):
    """Generate a complete Elisp file with the extracted knowledge."""
    
    with open(output_file, 'w') as f:
        f.write(";;; Generated Knowledge Base Facts\n")
        f.write(f";;; Generated on: {datetime.now().isoformat()}\n")
        f.write(";;; Generated using Outlines structured data extraction\n\n")
        
        f.write("(require 'kb-advanced-system)\n\n")
        
        # Initialize if needed
        f.write("(unless (boundp 'kb-current-mt)\n")
        f.write("  (kb-init))\n\n")
        
        # Set microtheory if specified
        if microtheory:
            f.write(f"(in-microtheory {microtheory}\n")
            indent = "  "
        else:
            indent = ""
        
        # Add regular facts
        if extraction_result.facts:
            f.write(f"{indent};; Regular facts\n")
            for fact in extraction_result.facts:
                f.write(f"{indent}{format_elisp_fact(fact)}\n")
            f.write("\n")
        
        # Add temporal facts
        if extraction_result.temporal_facts:
            f.write(f"{indent};; Temporal facts\n")
            for fact in extraction_result.temporal_facts:
                f.write(f"{indent}{format_elisp_temporal_fact(fact)}\n")
            f.write("\n")
        
        # Add events
        if extraction_result.events:
            f.write(f"{indent};; Events\n")
            for event in extraction_result.events:
                f.write(f"{indent}{format_elisp_event(event)}\n")
            f.write("\n")
        
        # Close microtheory block if needed
        if microtheory:
            f.write(")\n")
        
        # Trigger reasoning
        f.write(f"{indent};; Perform reasoning to infer new facts\n")
        f.write(f"{indent}(kb-reason)\n")

def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description='Generate KB facts using Outlines')
    parser.add_argument('--text', help='Text to extract facts from')
    parser.add_argument('--file', help='File containing text to process')
    parser.add_argument('--output', '-o', default='generated_facts.el', 
                       help='Output Elisp file (default: generated_facts.el)')
    parser.add_argument('--microtheory', '-m', help='Target microtheory for facts')
    parser.add_argument('--model', default='microsoft/Phi-3-mini-4k-instruct',
                       help='Model to use for generation')
    
    args = parser.parse_args()
    
    if not args.text and not args.file:
        print("Error: Must specify either --text or --file")
        sys.exit(1)
    
    # Get input text
    if args.file:
        try:
            with open(args.file, 'r') as f:
                input_text = f.read()
        except Exception as e:
            print(f"Error reading file {args.file}: {e}")
            sys.exit(1)
    else:
        input_text = args.text
    
    # Initialize generator
    generator = KBDataGenerator(args.model)
    
    # Extract facts
    print("Extracting structured facts...")
    result = generator.extract_facts(input_text, args.microtheory)
    
    # Generate output
    generate_elisp_file(result, args.output, args.microtheory)
    
    print(f"Generated {len(result.facts)} facts, {len(result.temporal_facts)} temporal facts, "
          f"and {len(result.events)} events")
    print(f"Output written to: {args.output}")
    
    # Show preview
    if result.facts:
        print("\nSample facts generated:")
        for fact in result.facts[:3]:
            print(f"  {fact.subject} --{fact.predicate}--> {fact.object}")

if __name__ == "__main__":
    main()
