#!/usr/bin/env python3
"""
Simple Knowledge Base Data Generator

A demonstration script showing how to structure data for the Elisp KB system.
This version doesn't require Outlines - it shows the data format and structure.

Usage:
    python kb_data_generator_simple.py --file input.txt --output facts.el
"""

import argparse
import re
from typing import List, Dict, Any
from datetime import datetime

class KBFact:
    """Simple representation of a KB fact."""
    def __init__(self, subject: str, predicate: str, obj: Any, 
                 certainty: float = 1.0, microtheory: str = None):
        self.subject = subject
        self.predicate = predicate
        self.object = obj
        self.certainty = certainty
        self.microtheory = microtheory

def extract_facts_simple(text: str, microtheory: str = None) -> List[KBFact]:
    """Extract facts using simple pattern matching."""
    facts = []
    
    # Pattern: X is a Y
    is_a_pattern = r'(\w+(?:\s+\w+)*)\s+(?:is|was)\s+an?\s+(\w+(?:\s+\w+)*)'
    for match in re.finditer(is_a_pattern, text, re.IGNORECASE):
        subject = match.group(1).replace(" ", "_")
        obj = match.group(2).replace(" ", "_")
        facts.append(KBFact(subject, "is-a", obj, 0.9, microtheory))
    
    # Pattern: X born in Y
    born_pattern = r'(\w+(?:\s+\w+)*)\s+(?:was\s+)?born\s+in\s+([^,\.]+?)(?:\s+in\s+(\d{4}))?[,\.]'
    for match in re.finditer(born_pattern, text, re.IGNORECASE):
        subject = match.group(1).replace(" ", "_")
        location = match.group(2).strip().replace(" ", "_")
        facts.append(KBFact(subject, "born-in", location, 0.85, microtheory))
        
        if match.group(3):  # Year
            year = int(match.group(3))
            facts.append(KBFact(subject, "birth-year", year, 0.9, microtheory))
    
    # Pattern: X worked at Y
    worked_pattern = r'(\w+(?:\s+\w+)*)\s+worked\s+at\s+([^,\.]+?)(?:\s+from\s+(\d{4})\s+(?:until|to)\s+(?:his\s+death\s+in\s+)?(\d{4}))?[,\.]'
    for match in re.finditer(worked_pattern, text, re.IGNORECASE):
        subject = match.group(1).replace(" ", "_")
        workplace = match.group(2).strip().replace(" ", "_")
        facts.append(KBFact(subject, "worked-at", workplace, 0.85, microtheory))
    
    # Pattern: X developed/created Y
    developed_pattern = r'(\w+(?:\s+\w+)*)\s+(?:developed|created)\s+(?:the\s+)?([^,\.]+?)[,\.]'
    for match in re.finditer(developed_pattern, text, re.IGNORECASE):
        subject = match.group(1).replace(" ", "_")
        creation = match.group(2).strip().replace(" ", "_")
        facts.append(KBFact(subject, "developed", creation, 0.8, microtheory))
    
    # Pattern: X received Y
    received_pattern = r'(\w+(?:\s+\w+)*)\s+received\s+(?:the\s+)?([^,\.]+?)\s+(?:in\s+(\d{4}))?[,\.]'
    for match in re.finditer(received_pattern, text, re.IGNORECASE):
        subject = match.group(1).replace(" ", "_")
        award = match.group(2).strip().replace(" ", "_")
        facts.append(KBFact(subject, "received", award, 0.9, microtheory))
    
    return facts

def format_elisp_fact(fact: KBFact) -> str:
    """Convert a KBFact to Elisp kb-assert format."""
    # Format object based on type
    if isinstance(fact.object, str):
        obj_str = f"'{fact.object}"
    elif isinstance(fact.object, bool):
        obj_str = "t" if fact.object else "nil"
    elif isinstance(fact.object, (int, float)):
        obj_str = str(fact.object)
    else:
        obj_str = f"'{fact.object}"
    
    # Build the assertion
    assertion = f"(kb-assert '{fact.subject} '{fact.predicate} {obj_str}"
    
    # Add certainty if not 1.0
    if fact.certainty != 1.0:
        assertion += f" {fact.certainty}"
    
    # Add microtheory if specified and certainty was added
    if fact.microtheory and fact.certainty != 1.0:
        assertion += f" nil '{fact.microtheory}"
    elif fact.microtheory:
        assertion += f" nil nil '{fact.microtheory}"
    
    assertion += ")"
    return assertion

def generate_elisp_file(facts: List[KBFact], output_file: str, microtheory: str = None):
    """Generate a complete Elisp file with the extracted knowledge."""
    
    with open(output_file, 'w') as f:
        f.write(";;; Generated Knowledge Base Facts\n")
        f.write(f";;; Generated on: {datetime.now().isoformat()}\n")
        f.write(";;; Simple pattern-based fact extraction\n\n")
        
        f.write("(require 'kb-advanced-system)\n\n")
        
        # Initialize if needed
        f.write(";; Initialize KB system if not already done\n")
        f.write("(unless (boundp 'kb-current-mt)\n")
        f.write("  (kb-init))\n\n")
        
        # Create microtheory if specified
        if microtheory:
            f.write(f";; Create and switch to microtheory {microtheory}\n")
            f.write(f"(unless (kb-get-microtheory '{microtheory})\n")
            f.write(f"  (kb-create-microtheory '{microtheory} 'CommonSenseMt))\n\n")
            f.write(f"(in-microtheory {microtheory}\n")
            indent = "  "
        else:
            indent = ""
        
        # Add facts
        if facts:
            f.write(f"{indent};; Extracted facts ({len(facts)} total)\n")
            for fact in facts:
                f.write(f"{indent}{format_elisp_fact(fact)}\n")
            f.write("\n")
        
        # Close microtheory block if needed
        if microtheory:
            f.write(")\n\n")
        
        # Trigger reasoning
        f.write(";; Perform reasoning to infer additional facts\n")
        if microtheory:
            f.write(f"(in-microtheory {microtheory}\n")
            f.write("  (kb-reason))\n")
        else:
            f.write("(kb-reason)\n")
        
        f.write("\n;; Display status\n")
        f.write("(kb-status)\n")

def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(description='Generate KB facts with simple extraction')
    parser.add_argument('--text', help='Text to extract facts from')
    parser.add_argument('--file', help='File containing text to process')
    parser.add_argument('--output', '-o', default='generated_facts.el', 
                       help='Output Elisp file (default: generated_facts.el)')
    parser.add_argument('--microtheory', '-m', help='Target microtheory for facts')
    
    args = parser.parse_args()
    
    if not args.text and not args.file:
        print("Error: Must specify either --text or --file")
        return
    
    # Get input text
    if args.file:
        try:
            with open(args.file, 'r') as f:
                input_text = f.read()
            print(f"Reading from file: {args.file}")
        except Exception as e:
            print(f"Error reading file {args.file}: {e}")
            return
    else:
        input_text = args.text
    
    # Extract facts
    print("Extracting facts using pattern matching...")
    facts = extract_facts_simple(input_text, args.microtheory)
    
    # Generate output
    generate_elisp_file(facts, args.output, args.microtheory)
    
    print(f"Generated {len(facts)} facts")
    print(f"Output written to: {args.output}")
    
    # Show preview
    if facts:
        print("\nExtracted facts:")
        for fact in facts:
            print(f"  {fact.subject} --{fact.predicate}--> {fact.object} (certainty: {fact.certainty})")

if __name__ == "__main__":
    main()
