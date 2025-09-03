#!/usr/bin/env python3
"""
OpenAI Knowledge Base Fact Generator using Outlines

Generates structured KB facts from text using OpenAI API + Outlines.

Usage:
    export OPENAI_API_KEY="your-key-here" 
    python kb_openai_generator.py --file input.txt --output facts.el
"""

import argparse
import os
import sys
from typing import List, Optional
from datetime import datetime

try:
    import outlines
    from pydantic import BaseModel, Field
    from openai import OpenAI
except ImportError:
    print("Error: Install with: pip install outlines[openai] pydantic openai")
    sys.exit(1)

class KBFact(BaseModel):
    """Knowledge base fact in subject-predicate-object format."""
    subject: str = Field(description="Entity name (use underscores, no spaces)")
    predicate: str = Field(description="Relationship (use dashes: is-a, works-at)")
    object: str = Field(description="Target entity or value")
    certainty: float = Field(default=0.9, ge=0.0, le=1.0)

class OpenAIKBGenerator:
    """Generate KB facts using OpenAI through Outlines."""
    
    def __init__(self, model: str = "gpt-4o-mini"):
        """Initialize with OpenAI model."""
        self.api_key = os.getenv("OPENAI_API_KEY")
        if not self.api_key:
            raise ValueError("Set OPENAI_API_KEY environment variable")
        
        try:
            self.client = OpenAI(api_key=self.api_key)
            self.model = outlines.from_openai(model, client=self.client)
            print(f"Initialized OpenAI: {model}")
        except Exception as e:
            print(f"Error: {e}")
            raise
    
    def extract_facts(self, text: str) -> List[KBFact]:
        """Extract facts from text."""
        prompt = f"""Extract knowledge base facts from this text as subject-predicate-object triplets.

Text: {text}

Extract factual information like:
- X is-a Y (classifications)  
- X born-in Y (locations)
- X works-at Y (employment)
- X created Y (authorship/invention)

Use simple names with underscores. Use dash-separated predicates.
"""
        
        try:
            result = self.model(prompt, List[KBFact], max_tokens=1000)
            return result
        except Exception as e:
            print(f"Extraction error: {e}")
            return []

def format_elisp_fact(fact: KBFact, microtheory: str = None) -> str:
    """Convert KBFact to Elisp kb-assert format."""
    assertion = f"(kb-assert '{fact.subject} '{fact.predicate} '{fact.object}"
    if fact.certainty != 0.9:
        assertion += f" {fact.certainty}"
    assertion += ")"
    return assertion

def generate_elisp_file(facts: List[KBFact], output_file: str, microtheory: str = None):
    """Generate Elisp file with KB facts."""
    with open(output_file, 'w') as f:
        f.write(";;; Generated KB Facts (OpenAI + Outlines)\n")
        f.write(f";;; Generated: {datetime.now().isoformat()}\n\n")
        f.write("(require 'kb-advanced-system)\n\n")
        f.write("(unless (boundp 'kb-current-mt) (kb-init))\n\n")
        
        if microtheory:
            f.write(f"(unless (kb-get-microtheory '{microtheory})\n")
            f.write(f"  (kb-create-microtheory '{microtheory} 'CommonSenseMt))\n\n")
            f.write(f"(in-microtheory {microtheory}\n")
            indent = "  "
        else:
            indent = ""
        
        f.write(f"{indent};; Generated facts ({len(facts)})\n")
        for fact in facts:
            f.write(f"{indent}{format_elisp_fact(fact)}\n")
        
        if microtheory:
            f.write(")\n")
        
        f.write("\n(kb-reason)\n")

def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description='Generate KB facts using OpenAI + Outlines')
    parser.add_argument('--text', help='Text to extract facts from')
    parser.add_argument('--file', help='File containing text to process')
    parser.add_argument('--output', '-o', default='openai_facts.el', 
                       help='Output Elisp file (default: openai_facts.el)')
    parser.add_argument('--microtheory', '-m', help='Target microtheory for facts')
    parser.add_argument('--model', default='gpt-4o-mini', help='OpenAI model')
    
    args = parser.parse_args()
    
    if not args.text and not args.file:
        print("Error: Must specify --text or --file")
        sys.exit(1)
    
    if not os.getenv("OPENAI_API_KEY"):
        print("Error: Set OPENAI_API_KEY environment variable")
        sys.exit(1)
    
    # Get input text
    if args.file:
        with open(args.file, 'r') as f:
            input_text = f.read()
    else:
        input_text = args.text
    
    # Extract facts
    generator = OpenAIKBGenerator(args.model)
    facts = generator.extract_facts(input_text)
    
    # Generate output
    generate_elisp_file(facts, args.output, args.microtheory)
    
    print(f"Generated {len(facts)} facts -> {args.output}")
    if facts:
        print("Sample facts:")
        for fact in facts[:3]:
            print(f"  {fact.subject} --{fact.predicate}--> {fact.object}")

if __name__ == "__main__":
    main()
