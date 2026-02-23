#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match self {
            Type::Int => "Int".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Unit => "Unit".to_string(),
            Type::Function {
                params,
                return_type,
            } => {
                let params_str: Vec<String> = params.iter().map(|p| format!("{}", p)).collect();
                format!("({}) -> {}", params_str.join(", "), *return_type)
            }
        };
        write!(f, "{}", s)
    }
}
