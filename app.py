import os
import re
import json
from flask import Flask, request, render_template, jsonify, send_file
from werkzeug.utils import secure_filename
from datetime import datetime
import zipfile
import io

app = Flask(__name__)
UPLOAD_FOLDER = 'uploads'
OUTPUT_FOLDER = 'output'
ALLOWED_EXTENSIONS = {'cob', 'cbl', 'txt'}

app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER
app.config['OUTPUT_FOLDER'] = OUTPUT_FOLDER

# Create directories if they don't exist
for folder in [UPLOAD_FOLDER, OUTPUT_FOLDER]:
    if not os.path.exists(folder):
        os.makedirs(folder)

def allowed_file(filename):
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS

class COBOLAnalyzer:
    def __init__(self):
        self.variables = {}
        self.procedures = []
        self.data_division = []
        self.working_storage = []
        self.file_section = []
        self.complexity_score = 0
        self.line_count = 0
        self.conversion_notes = []
        self.warnings = []
        
    def analyze_structure(self, content):
        lines = content.splitlines()
        self.line_count = len([line for line in lines if line.strip()])
        
        current_section = None
        in_data_division = False
        in_procedure_division = False
        
        for i, line in enumerate(lines, 1):
            line = line.strip()
            if not line or line.startswith('*'):
                continue
                
            # Identify divisions and sections
            if 'DATA DIVISION' in line.upper():
                in_data_division = True
                current_section = 'DATA'
            elif 'PROCEDURE DIVISION' in line.upper():
                in_procedure_division = True
                in_data_division = False
                current_section = 'PROCEDURE'
            elif 'WORKING-STORAGE SECTION' in line.upper():
                current_section = 'WORKING-STORAGE'
            elif 'FILE SECTION' in line.upper():
                current_section = 'FILE'
                
            # Analyze variable declarations
            if in_data_division and re.match(r'\d+\s+\w+', line):
                self.analyze_variable_declaration(line)
                
            # Analyze procedures and paragraphs
            if in_procedure_division and line.endswith('.') and not any(keyword in line.upper() for keyword in ['DISPLAY', 'ACCEPT', 'COMPUTE', 'MOVE', 'IF', 'PERFORM']):
                self.procedures.append(line[:-1])
                
            # Calculate complexity
            if any(keyword in line.upper() for keyword in ['IF', 'ELSE', 'PERFORM', 'CALL', 'GO TO']):
                self.complexity_score += 1
                
    def analyze_variable_declaration(self, line):
        # Parse COBOL variable declarations
        parts = line.split()
        if len(parts) >= 2:
            level = parts[0]
            name = parts[1]
            pic_clause = ''
            
            if 'PIC' in line.upper() or 'PICTURE' in line.upper():
                pic_match = re.search(r'PIC(?:TURE)?\s+([^\s.]+)', line.upper())
                if pic_match:
                    pic_clause = pic_match.group(1)
                    
            self.variables[name] = {
                'level': level,
                'picture': pic_clause,
                'type': self.determine_type(pic_clause),
                'line': line
            }
            
    def determine_type(self, pic_clause):
        if not pic_clause:
            return 'unknown'
        if 'X' in pic_clause:
            return 'string'
        elif '9' in pic_clause:
            if 'V' in pic_clause or '.' in pic_clause:
                return 'decimal'
            else:
                return 'integer'
        elif 'S' in pic_clause:
            return 'signed_numeric'
        return 'unknown'

class AdvancedCOBOLConverter:
    def __init__(self):
        self.analyzer = COBOLAnalyzer()
        self.python_code = []
        self.imports = set()
        self.functions = []
        self.main_code = []
        self.conversion_stats = {
            'statements_converted': 0,
            'unsupported_statements': 0,
            'variables_declared': 0,
            'functions_created': 0
        }
        
    def convert(self, content):
        # Analyze the COBOL structure first
        self.analyzer.analyze_structure(content)
        
        lines = content.splitlines()
        in_procedure_division = False
        current_paragraph = None
        
        # Add standard imports
        self.imports.add('import sys')
        self.imports.add('from decimal import Decimal')
        
        for line_num, line in enumerate(lines, 1):
            original_line = line
            line = line.strip()
            
            if not line or line.startswith('*'):
                continue
                
            if 'PROCEDURE DIVISION' in line.upper():
                in_procedure_division = True
                continue
                
            if not in_procedure_division:
                continue
                
            # Process COBOL statements
            try:
                converted = self.convert_statement(line, line_num)
                if converted:
                    self.main_code.append(f"    # Line {line_num}: {original_line.strip()}")
                    if isinstance(converted, list):
                        self.main_code.extend(f"    {stmt}" for stmt in converted)
                    else:
                        self.main_code.append(f"    {converted}")
                    self.conversion_stats['statements_converted'] += 1
                else:
                    self.main_code.append(f"    # TODO: Convert - {original_line.strip()}")
                    self.conversion_stats['unsupported_statements'] += 1
                    self.analyzer.warnings.append(f"Line {line_num}: Unsupported statement - {line}")
            except Exception as e:
                self.main_code.append(f"    # ERROR: {str(e)} - {original_line.strip()}")
                self.analyzer.warnings.append(f"Line {line_num}: Conversion error - {str(e)}")
                
        return self.generate_python_code()
        
    def convert_statement(self, line, line_num):
        line_upper = line.upper()
        
        # DISPLAY statement
        if 'DISPLAY' in line_upper:
            return self.convert_display(line)
            
        # ACCEPT statement
        elif 'ACCEPT' in line_upper:
            return self.convert_accept(line)
            
        # COMPUTE statement
        elif 'COMPUTE' in line_upper:
            return self.convert_compute(line)
            
        # MOVE statement
        elif 'MOVE' in line_upper:
            return self.convert_move(line)
            
        # IF statement
        elif line_upper.startswith('IF'):
            return self.convert_if(line)
            
        # PERFORM statement
        elif 'PERFORM' in line_upper:
            return self.convert_perform(line)
            
        # STOP RUN
        elif 'STOP RUN' in line_upper:
            return "sys.exit(0)"
            
        # ADD statement
        elif line_upper.startswith('ADD'):
            return self.convert_add(line)
            
        # SUBTRACT statement
        elif line_upper.startswith('SUBTRACT'):
            return self.convert_subtract(line)
            
        # MULTIPLY statement
        elif line_upper.startswith('MULTIPLY'):
            return self.convert_multiply(line)
            
        # DIVIDE statement
        elif line_upper.startswith('DIVIDE'):
            return self.convert_divide(line)
            
        return None
        
    def convert_display(self, line):
        # Handle DISPLAY statements with various formats
        match = re.search(r'DISPLAY\s+(.+)', line, re.IGNORECASE)
        if match:
            content = match.group(1).strip()
            
            # Handle quoted strings
            if content.startswith('"') and content.endswith('"'):
                return f'print({content})'
            elif content.startswith("'") and content.endswith("'"):
                return f'print({content})'
            else:
                # Handle variables
                variables = [var.strip() for var in content.split()]
                if len(variables) == 1:
                    return f'print({variables[0].lower()})'
                else:
                    var_list = ', '.join(variables)
                    return f'print({var_list.lower()})'
                    
        return None
        
    def convert_accept(self, line):
        match = re.search(r'ACCEPT\s+(\w+)', line, re.IGNORECASE)
        if match:
            var_name = match.group(1).lower()
            return f'{var_name} = input("Enter {var_name}: ")'
        return None
        
    def convert_compute(self, line):
        match = re.search(r'COMPUTE\s+(\w+)\s*=\s*(.+)', line, re.IGNORECASE)
        if match:
            var_name = match.group(1).lower()
            expression = match.group(2).replace('**', '^').lower()
            # Convert COBOL operators to Python
            expression = expression.replace('^', '**')
            return f'{var_name} = {expression}'
        return None
        
    def convert_move(self, line):
        match = re.search(r'MOVE\s+(.+)\s+TO\s+(.+)', line, re.IGNORECASE)
        if match:
            source = match.group(1).strip()
            target = match.group(2).strip().lower()
            
            if source.startswith('"') and source.endswith('"'):
                return f'{target} = {source}'
            elif source.startswith("'") and source.endswith("'"):
                return f'{target} = {source}'
            else:
                return f'{target} = {source.lower()}'
        return None
        
    def convert_if(self, line):
        # Basic IF statement conversion
        match = re.search(r'IF\s+(.+)\s+THEN', line, re.IGNORECASE)
        if match:
            condition = match.group(1).lower()
            condition = condition.replace(' = ', ' == ')
            return f'if {condition}:'
        return None
        
    def convert_perform(self, line):
        if 'UNTIL' in line.upper():
            match = re.search(r'PERFORM\s+UNTIL\s+(.+)', line, re.IGNORECASE)
            if match:
                condition = match.group(1).lower()
                condition = condition.replace(' = ', ' == ')
                return f'while not ({condition}):'
        else:
            match = re.search(r'PERFORM\s+(\w+)', line, re.IGNORECASE)
            if match:
                procedure_name = match.group(1).lower()
                return f'{procedure_name}()'
        return None
        
    def convert_add(self, line):
        match = re.search(r'ADD\s+(.+)\s+TO\s+(\w+)', line, re.IGNORECASE)
        if match:
            value = match.group(1).strip().lower()
            target = match.group(2).strip().lower()
            return f'{target} += {value}'
        return None
        
    def convert_subtract(self, line):
        match = re.search(r'SUBTRACT\s+(.+)\s+FROM\s+(\w+)', line, re.IGNORECASE)
        if match:
            value = match.group(1).strip().lower()
            target = match.group(2).strip().lower()
            return f'{target} -= {value}'
        return None
        
    def convert_multiply(self, line):
        match = re.search(r'MULTIPLY\s+(.+)\s+BY\s+(\w+)', line, re.IGNORECASE)
        if match:
            value = match.group(1).strip().lower()
            target = match.group(2).strip().lower()
            return f'{target} *= {value}'
        return None
        
    def convert_divide(self, line):
        match = re.search(r'DIVIDE\s+(.+)\s+INTO\s+(\w+)', line, re.IGNORECASE)
        if match:
            value = match.group(1).strip().lower()
            target = match.group(2).strip().lower()
            return f'{target} /= {value}'
        return None
        
    def generate_python_code(self):
        result = []
        
        # Add header comment
        result.append("#!/usr/bin/env python3")
        result.append('"""')
        result.append("Converted from COBOL to Python")
        result.append(f"Conversion Date: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        result.append(f"Original COBOL Lines: {self.analyzer.line_count}")
        result.append(f"Variables Found: {len(self.analyzer.variables)}")
        result.append(f"Procedures Found: {len(self.analyzer.procedures)}")
        result.append('"""')
        result.append("")
        
        # Add imports
        for imp in sorted(self.imports):
            result.append(imp)
        result.append("")
        
        # Add variable declarations based on COBOL analysis
        if self.analyzer.variables:
            result.append("# Variable declarations (based on COBOL DATA DIVISION)")
            for var_name, var_info in self.analyzer.variables.items():
                var_type = var_info['type']
                if var_type == 'string':
                    result.append(f"{var_name.lower()} = ''")
                elif var_type in ['integer', 'signed_numeric']:
                    result.append(f"{var_name.lower()} = 0")
                elif var_type == 'decimal':
                    result.append(f"{var_name.lower()} = Decimal('0.0')")
                else:
                    result.append(f"{var_name.lower()} = None  # {var_info['picture']}")
            result.append("")
            
        # Add function definitions for procedures
        if self.analyzer.procedures:
            result.append("# Procedure definitions")
            for proc in self.analyzer.procedures:
                result.append(f"def {proc.lower().replace('-', '_')}():")
                result.append("    # TODO: Implement procedure logic")
                result.append("    pass")
                result.append("")
                
        # Add main function
        result.append("def main():")
        result.append("    \"\"\"Main program logic\"\"\"")
        
        if self.main_code:
            result.extend(self.main_code)
        else:
            result.append("    # No convertible COBOL statements found")
            result.append("    pass")
            
        result.append("")
        result.append("if __name__ == '__main__':")
        result.append("    main()")
        
        return "\n".join(result)
        
    def get_analysis_report(self):
        return {
            'structure_analysis': {
                'total_lines': self.analyzer.line_count,
                'variables_count': len(self.analyzer.variables),
                'procedures_count': len(self.analyzer.procedures),
                'complexity_score': self.analyzer.complexity_score,
                'variables': self.analyzer.variables,
                'procedures': self.analyzer.procedures
            },
            'conversion_stats': self.conversion_stats,
            'warnings': self.analyzer.warnings,
            'recommendations': self.generate_recommendations()
        }
        
    def generate_recommendations(self):
        recommendations = []
        
        if self.conversion_stats['unsupported_statements'] > 0:
            recommendations.append("Review unsupported statements marked with TODO comments")
            
        if self.analyzer.complexity_score > 10:
            recommendations.append("Consider breaking down complex logic into smaller functions")
            
        if len(self.analyzer.variables) > 20:
            recommendations.append("Consider using classes or data structures to organize variables")
            
        if not self.analyzer.procedures:
            recommendations.append("Consider adding functions to improve code organization")
            
        return recommendations

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/convert', methods=['POST'])
def convert_file():
    try:
        if 'file' not in request.files:
            return jsonify({'error': 'No file selected'}), 400
            
        file = request.files['file']
        if file.filename == '':
            return jsonify({'error': 'No file selected'}), 400
            
        if not allowed_file(file.filename):
            return jsonify({'error': 'Please upload a valid COBOL file (.cob, .cbl, or .txt)'}), 400
            
        # Save uploaded file
        filename = secure_filename(file.filename)
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        unique_filename = f"{timestamp}_{filename}"
        filepath = os.path.join(app.config['UPLOAD_FOLDER'], unique_filename)
        file.save(filepath)
        
        # Read and convert
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            cobol_content = f.read()
            
        converter = AdvancedCOBOLConverter()
        python_code = converter.convert(cobol_content)
        analysis_report = converter.get_analysis_report()
        
        # Save Python output
        python_filename = f"{timestamp}_{filename.rsplit('.', 1)[0]}.py"
        python_filepath = os.path.join(app.config['OUTPUT_FOLDER'], python_filename)
        with open(python_filepath, 'w', encoding='utf-8') as f:
            f.write(python_code)
            
        return jsonify({
            'success': True,
            'python_code': python_code,
            'analysis_report': analysis_report,
            'python_filename': python_filename,
            'original_filename': filename
        })
        
    except Exception as e:
        return jsonify({'error': f'Error processing file: {str(e)}'}), 500

@app.route('/download/<filename>')
def download_file(filename):
    try:
        filepath = os.path.join(app.config['OUTPUT_FOLDER'], filename)
        return send_file(filepath, as_attachment=True)
    except Exception as e:
        return jsonify({'error': f'File not found: {str(e)}'}), 404

@app.route('/download_package/<original_filename>')
def download_package(original_filename):
    try:
        # Create a zip file with all related files
        memory_file = io.BytesIO()
        
        with zipfile.ZipFile(memory_file, 'w', zipfile.ZIP_DEFLATED) as zf:
            # Find related files
            base_name = original_filename.rsplit('.', 1)[0]
            
            for file in os.listdir(app.config['OUTPUT_FOLDER']):
                if base_name in file:
                    file_path = os.path.join(app.config['OUTPUT_FOLDER'], file)
                    zf.write(file_path, file)
                    
        memory_file.seek(0)
        
        return send_file(
            io.BytesIO(memory_file.read()),
            mimetype='application/zip',
            as_attachment=True,
            download_name=f'{base_name}_converted.zip'
        )
        
    except Exception as e:
        return jsonify({'error': f'Error creating package: {str(e)}'}), 500

if __name__ == '__main__':
    print("Starting Enhanced COBOL to Python Converter...")
    print("Features:")
    print("- Advanced COBOL statement conversion")
    print("- Detailed code analysis and reporting")
    print("- Variable type detection")
    print("- Procedure identification")
    print("- Complexity analysis")
    print("- Download converted files")
    print("Visit: http://127.0.0.1:5000")
    app.run(debug=True, host='127.0.0.1', port=5000)