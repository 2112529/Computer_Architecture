#include <stdio.h>
#include "shell.h"
int32_t signExtend(uint32_t imm) {
    // Left shift by 2 bits
    // Check the original sign bit (now at position 2) and sign-extend if necessary
    if (imm >= 0x00008000) { // Check if the 16th bit is set
        imm |= 0xFFFF0000;  // Sign-extend
    }
    imm <<= 2;

    return (int32_t)imm;   // Cast to signed integer for proper sign extension
}
uint32_t signExtend16bits(uint16_t imm) {
    int32_t signed_imm = *((int16_t*)&imm);

    uint32_t extended_imm = *((uint32_t*)&signed_imm);
    // 返回32位无符号整数的符号扩展值
    return extended_imm;
}
//signExtendByte函数实现将一个uint8_t变量符号拓展为32位，然后返回一个uint32_t变量
uint32_t signExtendByte(uint8_t byte) {
    // 将uint8_t转换为int8_t
    int8_t signedByte = (int8_t)byte;
    
    // 将int8_t转换为int32_t，编译器会自动进行符号扩展
    int32_t extended = (int32_t)signedByte;
    
    // 返回uint32_t值
    return (uint32_t)extended;
}
uint32_t zeroExtend32(uint32_t imm) { return imm; }

uint32_t zeroExtend8(uint8_t imm) { return imm; }

uint32_t zeroExtend16(uint16_t imm) { return imm; }
void process_instruction()
{
    /* execute one instruction here. You should use CURRENT_STATE and modify
     * values in NEXT_STATE. You can call mem_read_32() and mem_write_32() to
     * access memory. */
    //printf("Test for my first ");
    // char buffer[8];
    // if(fgets(buffer,sizeof(buffer),stdin)!= NULL)
    //     printf("你输入的字符串是：%s", buffer);
    printf("PC: 0x%08X\n", CURRENT_STATE.PC);
    uint32_t instr=mem_read_32(CURRENT_STATE.PC);
    printf("PC 指向的地址内容为: 0x%08X\n", instr);
    //printf(address);
    uint32_t mask = 0xFC000000; // This is a bitmask for the top 6 bits: 11111100 00000000 00000000 00000000
    uint32_t op = (instr & mask) >> 26;
    uint32_t rs = (instr & 0x03E00000) >> 21;
    uint32_t rt = (instr & 0x001F0000) >> 16;
    uint32_t rd = (instr & 0x0000F800) >> 11;
    uint32_t shamt = (instr & 0x000007C0) >> 6;//lower 5
    uint32_t funct = (instr & 0x0000003F);//lowest 6
    uint32_t imm = (instr & 0x0000FFFF);//16
    uint32_t target = (instr & 0x03FFFFFF);//26
    //uint32_t offset = (instr & 0x0000FFFF);
    printf("target: 0x%08X\n", target);
    printf("instr: 0x%08X\n", instr);
    printf("top6bits: 0x%02X\n", op);
    switch(op){
        case 0x09:
            printf("addiu instr\n");
            NEXT_STATE.REGS[rd]=CURRENT_STATE.REGS[rs]+imm;
            NEXT_STATE.PC=CURRENT_STATE.PC+4;
            break;
        case 0x02:
            printf("j instr\n");
            NEXT_STATE.PC=(CURRENT_STATE.PC&0xF0000000)+(target<<2);
            printf("CURRENT_STATE.PC&0xF0000000: 0x%08X\n", CURRENT_STATE.PC&0xF0000000);
            printf("target<<2: 0x%08X\n", target<<2);
            printf("nextpc: 0x%08X\n", NEXT_STATE.PC);
            break;
        case 0x00:
            printf("special instr\n");
            switch (funct)
            {
            case 0x00:
                printf("sll instr\n");
                NEXT_STATE.REGS[rd]=CURRENT_STATE.REGS[rt]<< shamt;
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x02:
                printf("srl instr\n");
                NEXT_STATE.REGS[rd]=CURRENT_STATE.REGS[rt]>> shamt;
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x03:
            {
                printf("sra instr\n");  
                int32_t val = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                val = val >> shamt;
                NEXT_STATE.REGS[rd] = val;
                NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                break;
            }
                
            case 0x04:
                printf("sllv instr\n");
                NEXT_STATE.REGS[rd]=CURRENT_STATE.REGS[rt]<< CURRENT_STATE.REGS[shamt];
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x06:
                printf("srlv instr\n");
                NEXT_STATE.REGS[rd]=CURRENT_STATE.REGS[rt]>> CURRENT_STATE.REGS[shamt];
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x07:
            {
                printf("srav instr\n");
                int32_t val = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                uint32_t shamt = CURRENT_STATE.REGS[rs] & 0x1f;
                val = val >> shamt;
                NEXT_STATE.REGS[rd] = val;
                NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                break;
            }
                
            case 0x08:
                printf("jr instr\n");
                NEXT_STATE.PC=CURRENT_STATE.REGS[rs];
                break;
            case 0x09:
                printf("jalr instr\n");
                NEXT_STATE.REGS[rd]=CURRENT_STATE.PC+4;
                NEXT_STATE.PC=CURRENT_STATE.REGS[rs];
                break;
            case 0x0C:
                printf("syscall instr\n");
                //mips syscall
                if (CURRENT_STATE.REGS[2] == 0x0a) {
                        RUN_BIT = FALSE;
                    } 
                else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                break;
            case 0x10:
                printf("mfhi instr\n");
                NEXT_STATE.REGS[rd]=CURRENT_STATE.HI;
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x11:
                printf("mthi instr\n");
                CURRENT_STATE.HI=CURRENT_STATE.REGS[rs];
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x12:
                printf("mflo instr\n");
                NEXT_STATE.REGS[rd]=CURRENT_STATE.LO;
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x13:
                printf("mtlo instr\n");
                CURRENT_STATE.LO=CURRENT_STATE.REGS[rs];
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x18:
            {
                printf("mult instr\n");
                //mips mult
                int64_t lhs = *((int32_t*)&CURRENT_STATE.REGS[rs]);
                int64_t rhs = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                int64_t product = lhs * rhs;
                uint64_t uint_product = *((uint32_t*)&product);
                NEXT_STATE.HI =
                    (uint32_t)((uint_product >> 32) & 0xffffffff);
                NEXT_STATE.LO = (uint32_t)(uint_product & 0xffffffff);
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            }
                
            case 0x19:
            {
                printf("multu instr\n");
                uint64_t lhs = CURRENT_STATE.REGS[rs];
                uint64_t rhs = CURRENT_STATE.REGS[rt];
                uint64_t product = lhs * rhs;

                NEXT_STATE.HI = (uint32_t)((product >> 32) & 0xffffffff);
                NEXT_STATE.LO = (uint32_t)(product & 0xffffffff);
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            }
                
            case 0x1a:
            {
                printf("div instr\n");
                int32_t lhs = *((int32_t*)&CURRENT_STATE.REGS[rs]);
                int32_t rhs = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                NEXT_STATE.LO = lhs / rhs;
                NEXT_STATE.HI = lhs % rhs;
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            }
                
            case 0x1b:
            {
                printf("divu instr\n");
                uint32_t lhs = CURRENT_STATE.REGS[rs];
                uint32_t rhs = CURRENT_STATE.REGS[rt];
                NEXT_STATE.LO = lhs / rhs;
                NEXT_STATE.HI = lhs % rhs;
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            }
                
            
            case 0x20:
                printf("add instr\n");
                NEXT_STATE.REGS[rd] =CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x21:
                printf("addu instr\n");
                NEXT_STATE.REGS[rd] =CURRENT_STATE.REGS[rs] + CURRENT_STATE.REGS[rt];
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x22:
                printf("sub instr\n");
                NEXT_STATE.REGS[rd] =CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt];
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x23:
                printf("subu instr\n");
                NEXT_STATE.REGS[rd] =CURRENT_STATE.REGS[rs] - CURRENT_STATE.REGS[rt];
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x24:
                printf("and instr\n");
                NEXT_STATE.REGS[rd] =CURRENT_STATE.REGS[rs] & CURRENT_STATE.REGS[rt];
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x25:
                printf("or instr\n");
                NEXT_STATE.REGS[rd] =CURRENT_STATE.REGS[rs] | CURRENT_STATE.REGS[rt];
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x26:
                printf("xor instr\n");
                NEXT_STATE.REGS[rd] =CURRENT_STATE.REGS[rs] ^ CURRENT_STATE.REGS[rt];
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x27:
                printf("nor instr\n");
                NEXT_STATE.REGS[rd] =~(CURRENT_STATE.REGS[rs] ^ CURRENT_STATE.REGS[rt]);
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            case 0x2a:
            {
                printf("slt instr\n");  
                int32_t lhs = *((int32_t*)&CURRENT_STATE.REGS[rs]);
                int32_t rhs = *((int32_t*)&CURRENT_STATE.REGS[rt]);
                NEXT_STATE.REGS[rd] = (lhs < rhs) ? 1 : 0;
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            }
                
            case 0x2b:
                printf("sltu instr\n");
                NEXT_STATE.REGS[rd] =CURRENT_STATE.REGS[rs] < CURRENT_STATE.REGS[rt] ? 1 : 0;
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
                break;
            default:
                printf("unknown instruction\n");
                break;
            }
            break;
        case 0x01:
            printf("regimm instr\n");
            switch (rt)
            {
                case 0x0: {
                    printf("bltz instr\n");
                    if ((CURRENT_STATE.REGS[rs] & 0x80000000) != 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + signExtend(imm) + 4;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
                case 0x10: {
                    printf("bltzal instr\n");
                    NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
                    if ((CURRENT_STATE.REGS[rs] & 0x80000000) != 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + signExtend(imm) + 4;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
                case 0x1: {
                    printf("bgez instr\n");
                    if ((CURRENT_STATE.REGS[rs] & 0x80000000) == 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + signExtend(imm) + 4;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
                case 0x11: {
                    printf("bgezal instr\n");
                    NEXT_STATE.REGS[31] = CURRENT_STATE.PC + 4;
                    if ((CURRENT_STATE.REGS[rs] & 0x80000000) == 0) {
                        NEXT_STATE.PC = CURRENT_STATE.PC + signExtend(imm) + 4;
                    } else {
                        NEXT_STATE.PC = CURRENT_STATE.PC + 4;
                    }
                    break;
                }
                default:
                printf("Unknown instruction: 0x%x\n",instr);
                break;
            }
            
            break;
        case 0x03:
            printf("jal instr\n");
            NEXT_STATE.REGS[31]=CURRENT_STATE.PC+4;
            NEXT_STATE.PC=CURRENT_STATE.PC&0xF0000000+(target<<2);
            break;
        case 0x04:
            printf("beq instr\n");
            //mips beq
            if (CURRENT_STATE.REGS[rs]==CURRENT_STATE.REGS[rt])
                {imm=signExtend(imm);
                NEXT_STATE.PC=CURRENT_STATE.PC+imm+4;}
            else
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
            break;
        case 0x05:
            printf("bne instr\n");
            if (CURRENT_STATE.REGS[rs]!=CURRENT_STATE.REGS[rt]
                && CURRENT_STATE.REGS[rs]!=0x00000000
                && CURRENT_STATE.REGS[rt]!=0x00000000
                && CURRENT_STATE.REGS[rs]!=0xFFFFFFFF
                && CURRENT_STATE.REGS[rt]!=0xFFFFFFFF)
            {
                /// Sign extend the uint32_t imm and then shift by 2 bits
                imm=signExtend(imm);
                NEXT_STATE.PC=CURRENT_STATE.PC+imm+4;
            }
            else
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
            break;
        case 0x06:
            printf("blez instr\n"); 
            if (CURRENT_STATE.REGS[rs]<=0)
                {imm=signExtend(imm);
                NEXT_STATE.PC=CURRENT_STATE.PC+imm;}
            else
                NEXT_STATE.PC=CURRENT_STATE.PC+4;

            break;
        case 0x07:
            printf("bgtz instr\n");
            if (CURRENT_STATE.REGS[rs]>0)
                {imm=signExtend(imm);
                NEXT_STATE.PC=CURRENT_STATE.PC+imm;}
            else
                NEXT_STATE.PC=CURRENT_STATE.PC+4;
            break; 
        case 0x08:
            printf("addi instr\n");
            //mips addi
            NEXT_STATE.REGS[rt]=CURRENT_STATE.REGS[rs]+imm;
            NEXT_STATE.PC=CURRENT_STATE.PC+4;
            break;
        case 0x0A:
            printf("slti instr\n");

            //mips slti
            if (CURRENT_STATE.REGS[rs]<imm)
                NEXT_STATE.REGS[rt]=1;
            else
                NEXT_STATE.REGS[rt]=0;
            NEXT_STATE.PC=CURRENT_STATE.PC+4;

            break;
        case 0x0B:
            printf("sltiu instr\n"); 
            //mips sltiu
            if (CURRENT_STATE.REGS[rs]<imm)
                NEXT_STATE.REGS[rt]=1;
            else
                NEXT_STATE.REGS[rt]=0;
            NEXT_STATE.PC=CURRENT_STATE.PC+4;
            break;
        case 0x0C:
            printf("andi instr\n");
            //mips andi
            NEXT_STATE.REGS[rt]=CURRENT_STATE.REGS[rs]&imm;
            NEXT_STATE.PC=CURRENT_STATE.PC+4;
            break;
        case 0x0D:
            printf("ori instr\n"); 
            //mips ori
            NEXT_STATE.REGS[rt]=CURRENT_STATE.REGS[rs]|imm;
            NEXT_STATE.PC=CURRENT_STATE.PC+4;
            break;
        case 0x0E:
            printf("xori instr\n"); 
            //mips xori
            NEXT_STATE.REGS[rt]=CURRENT_STATE.REGS[rs]^imm;
            NEXT_STATE.PC=CURRENT_STATE.PC+4;
            break;
        case 0x0F:
            printf("lui instr\n");
            //mips lui
            NEXT_STATE.REGS[rt]=imm<<16;
            NEXT_STATE.PC=CURRENT_STATE.PC+4;
            break;
        case 0x20:
        {
            printf("lb instr\n"); 
            uint32_t addr = signExtend(imm) + CURRENT_STATE.REGS[rs];

            uint8_t byte = mem_read_32(addr) & 0xff;

            NEXT_STATE.REGS[rt] = signExtendByte(byte);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
         
            
        case 0x21:
        {
            printf("lh instr\n"); 
            //mips lh
            uint32_t addr = signExtend(imm) + CURRENT_STATE.REGS[rs];

            uint16_t lower16bits = mem_read_32(addr) & 0xffff;

            NEXT_STATE.REGS[rt] = signExtend16bits(lower16bits);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
            
        case 0x23:
            printf("lw instr\n"); 
            //mips lw
            NEXT_STATE.REGS[rt]=mem_read_32(CURRENT_STATE.REGS[rs]+imm);
            NEXT_STATE.PC=CURRENT_STATE.PC+4;
            break;
        case 0x24:
        {
            printf("lbu instr\n"); 
            //32 bits mips lbu
            uint32_t addr = signExtend(imm) + CURRENT_STATE.REGS[rs];

            uint8_t byte = mem_read_32(addr) & 0xff;

            NEXT_STATE.REGS[rt] = zeroExtend8(byte);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
            
        case 0x25:
        {
            printf("lhu instr\n");
            uint32_t addr = signExtend(imm) + CURRENT_STATE.REGS[rs];

            uint16_t half = mem_read_32(addr) & 0xffff;

            NEXT_STATE.REGS[rt] = zeroExtend16(half);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
            
        case 0x28:
        {
            printf("sb instr\n");
            //mips sb
            uint32_t addr = signExtend(imm) + CURRENT_STATE.REGS[rs];

            uint32_t val = (mem_read_32(addr) & 0xffffff00) |
                           (CURRENT_STATE.REGS[rt] & 0xff);

            mem_write_32(addr, val);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
            
        case 0x29:
        {
            printf("sh instr\n");
            uint32_t addr = signExtend(imm) + CURRENT_STATE.REGS[rs];

            uint32_t val = (mem_read_32(addr) & 0xffff0000) |
                           (CURRENT_STATE.REGS[rt] & 0xffff);
            mem_write_32(addr, val);
            NEXT_STATE.PC = CURRENT_STATE.PC + 4;
            break;
        }
            
        case 0x2B:
            printf("sw instr\n");
            //mips sw
            mem_write_32(CURRENT_STATE.REGS[rs]+imm,CURRENT_STATE.REGS[rt]);
            NEXT_STATE.PC=CURRENT_STATE.PC+4;
            break;
        default:
            printf("Unknown instruction\n");
            break;
          
    }
    //for debug
    char ch;
    ch = getchar();

    
        



    
}
