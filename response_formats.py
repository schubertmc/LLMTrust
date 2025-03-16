from pydantic import BaseModel, Field # type: ignore
from typing import List, Literal, Optional # type: ignore

#### Output formats###

class OpenPostTrustEvaluation(BaseModel):
    author_type: str = Field(description="Author type: Patient, Relative, Healthcare Professional, or Other")
    cancer_type: str = Field(description="Type of cancer discussed in the post, or 'unknown'")
    sex: str = Field(description="Male, Female, or Unknown")
    disease_stage: str = Field(description="Stage of the disease, or 'unknown'")
    tone: List[str] = Field(description="List of tone(s) of the post")
    depression_score: int = Field(description="Score of depression mentioned in the post, from 0 to 5")
    anxiety_score: int = Field(description="Anxiety score of the post, from 0 to 5")
    distress_score: int = Field(description="Distress score of the post, from 0 to 5")
    mistrust_trust_NA: Literal["mistrust", "trust", "NA"] = Field(description="Indicate whether the post expresses mistrust, trust, or lacks sufficient information to infer trust or mistrust toward the healthcare system.")
    object_of_mistrust: str = Field(description="The specific entity, individual, or system that is the focus of mistrust, if any.")
    mistrust_reason: str = Field(description="Underlying reason for the mistrust, if any")


class FinalPostTrustEvaluation(BaseModel):
    author_type: str = Field(description="Author type: Patient, Relative, Healthcare Professional, or Other")
    cancer_type: str = Field(description="Type of cancer discussed in the post, or 'unknown'")
    sex: str = Field(description="Sex of the author: Male, Female, or Unknown")
    disease_stage: str = Field(description="Stage of the disease, or 'unknown'")
    tone: Literal[
        "anxious",
        "concerned",
        "inquisitive",
        "hopeful",
        "worried",
        "frustrated",
        "uncertain",
        "informative",
        "supportive",
        "curious"
    ] = Field(description="Primary tone of the post")
    depression_score: int = Field(description="Score of depression mentioned in the post, from 0 to 5")
    anxiety_score: int = Field(description="Anxiety score of the post, from 0 to 5")
    distress_score: int = Field(description="Distress score of the post, from 0 to 5")
    mistrust_trust_NA: Literal["mistrust", "trust", "NA"] = Field(description="Indicate whether the post expresses mistrust, trust, or lacks sufficient information to infer trust or mistrust toward the healthcare system.")
    object_of_mistrust: str = Field(description="The specific entity, individual, or system that is the focus of mistrust, if any.")
    mistrust_reason: str = Field(description="Underlying reason for the mistrust, if any")
    object_of_mistrust_category: Literal[
        "Healthcare Professionals", 
        "Healthcare Institutions", 
        "Insurance Providers", 
        "Medical Science", 
        "Other"
    ] = Field(description="Category of the primary object of mistrust")
    mistrust_reason_category: Literal[
        "Communication",
        "Perceived Incompetence of Medical Management",
        "Disregard for Patient Concerns",
        "Profit-Driven Medical Management",
        "Lack of Trust in Medical Procedures", 
        "Other"
    ] = Field(description="Overall category of the primary reason for mistrust")


class CommunicationEvaluation(BaseModel):
    category: Literal[ "Unclear communication", 
                                   "Conflicting information", 
                                   "Perceived lack of empathy", 
                                   "Lack of Integrity", 
                                   "Other"] = Field(description="Type of communication issue")
    




class PerceivedIncompetenceEvaluation(BaseModel):
    category: Literal[ "Lack of knowledge/competency", 
                                   "Inadequate actions - perceived lack of thoroughness", 
                                   "Inadequate actions - Inadequate pain management", 
                                   "Inadequate actions - Delayed care", 
                                   "Inadequate actions - Potential overtreatment", 
                                   "Inadequate actions - Misdiagnoses",
                                    "Inadequate actions - Other",
                                   "Other"] = Field(description="Type of perceived incompetence")



class DisregardEvaluation(BaseModel):
    category: Literal[ "Symptoms", 
                                   "Treatment wishes/expectations", 
                                   "Mental health problems", 
                                   "Other medical conditions", 
                                   "Other"] = Field(description="Type of disregard")



class ProfitDrivenEvaluation(BaseModel):
    category: Literal[ "By Insurance", 
                                   "By Health care providers", 
                                   "Other"] = Field(description="Type of profit-driven medical management")




class LackOfTrustEvaluation(BaseModel):
    category: Literal[ "Treatment", 
                                   "Diagnosis", 
                                   "Lack of prioritization of cancer research", 
                                   "General diffuse mistrust of medical procedures",
                                   "Other"] = Field(description="Type of lack of trust in medical procedures")



# added 
class ToneAnalysis(BaseModel):
    tone: Literal[
        "anxious",
        "concerned",
        "inquisitive",
        "hopeful",
        "worried",
        "frustrated",
        "uncertain",
        "informative",
        "supportive",
        "curious",
        "reflective",
        "confused",
        "frustration", 
        "seeking support",
        "neutral",
        "concern",
        "fearful",
        "sad",
        "grateful",
        "optimistic",
        "other"
    ] = Field(description="Primary tone of the post")

