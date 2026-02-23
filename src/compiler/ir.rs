use crate::compiler::common::Location;

#[derive(Debug, Clone, PartialEq)]
pub struct IRVar {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Label {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub kind: InstructionKind,
    pub location: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstructionKind {
    LoadIntConst {
        value: i32,
        dest: IRVar,
    },
    LoadBoolConst {
        value: bool,
        dest: IRVar,
    },
    Copy {
        source: IRVar,
        dest: IRVar,
    },
    Call {
        fun: IRVar,
        args: Vec<IRVar>,
        dest: IRVar,
    },
    Jump {
        label: Label,
    },
    CondJump {
        cond: IRVar,
        then_label: Label,
        else_label: Label,
    },
    Label {
        label: Label,
    },
}

impl Instruction {
    pub fn load_int_const(value: i32, dest: IRVar, location: Location) -> Self {
        Instruction {
            kind: InstructionKind::LoadIntConst { value, dest },
            location,
        }
    }
    pub fn load_bool_const(value: bool, dest: IRVar, location: Location) -> Self {
        Instruction {
            kind: InstructionKind::LoadBoolConst { value, dest },
            location,
        }
    }
    pub fn copy(source: IRVar, dest: IRVar, location: Location) -> Self {
        Instruction {
            kind: InstructionKind::Copy { source, dest },
            location,
        }
    }
    pub fn call(fun: IRVar, args: Vec<IRVar>, dest: IRVar, location: Location) -> Self {
        Instruction {
            kind: InstructionKind::Call { fun, args, dest },
            location,
        }
    }
    pub fn jump(label: Label, location: Location) -> Self {
        Instruction {
            kind: InstructionKind::Jump { label },
            location,
        }
    }
    pub fn cond_jump(
        cond: IRVar,
        then_label: Label,
        else_label: Label,
        location: Location,
    ) -> Self {
        Instruction {
            kind: InstructionKind::CondJump {
                cond,
                then_label,
                else_label,
            },
            location,
        }
    }
    pub fn label(name: String, location: Location) -> Self {
        Instruction {
            kind: InstructionKind::Label {
                label: Label { name },
            },
            location,
        }
    }
}

impl std::fmt::Display for IRVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self.kind {
            InstructionKind::LoadIntConst { value, dest } => {
                write!(f, "LoadIntCost({}, {})", value, dest)
            }
            InstructionKind::LoadBoolConst { value, dest } => {
                write!(f, "LoadBoolCost({}, {})", value, dest)
            }
            InstructionKind::Copy { source, dest } => {
                write!(f, "Copy({}, {})", source, dest)
            }
            InstructionKind::Call { fun, args, dest } => {
                write!(f, "Call({}, {:?}, {})", fun, args, dest)
            }
            InstructionKind::Jump { label } => {
                write!(f, "Jump({})", label)
            }
            InstructionKind::CondJump {
                cond,
                then_label,
                else_label,
            } => {
                write!(f, "CondJump({}, {}, {})", cond, then_label, else_label)
            }
            InstructionKind::Label { label } => {
                write!(f, "Label({})", label)
            }
        }
    }
}
