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
