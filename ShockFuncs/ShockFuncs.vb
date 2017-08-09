' 2010 Spiritech Advanced Products, Inc
Imports System.Math
Public Class ShockFuncs

    Public Enum AnglesIn
        NotSet = 0
        Radians = 1
        Degrees = 2
    End Enum

    Public Enum ShockStrength
        NotSet = 0
        Weak = 1
        Strong = 2
    End Enum

    Public Shared Function ShockDeflect_fAngle(ByVal Gamma As Double, ByVal M1 As Double, ByVal ShockAngle As Double, ByVal AngleIn As AnglesIn) As Double
        'calculates the deflection angle across a shock wave given the upstream mach number, gamma, and the oblique shock angle

        Dim Delta As Double
        Dim AngR As Double

        If AngleIn = AnglesIn.Degrees Then
            AngR = ShockAngle * PI / 180
        Else
            AngR = ShockAngle
        End If

        If AngR > PI / 2 Then Return 0.0 'non-realistic solution

        Delta = Atan((M1 ^ 2 * Sin(2 * AngR) - 2 * Tan(PI / 2 - AngR)) / (2 + M1 ^ 2 * (Gamma + Cos(2 * AngR))))
        If Delta < 0 Then Delta = 0.0 'such a weak shock that flow does not turn

        If AngleIn = AnglesIn.Degrees Then
            Return Delta * 180 / PI
        Else
            Return Delta
        End If

    End Function

    Public Shared Function ShockDeflect_fP2P1(ByVal Gamma As Double, ByVal M1 As Double, ByVal Ps2Ps1 As Double, ByVal AngleIn As AnglesIn) As Double
        'calculates the deflection angle of the shock from the static pressure ratio

        Dim gp1, gm1 As Double
        Dim Delta As Double
        gp1 = Gamma + 1
        gm1 = Gamma - 1
        Delta = Atan((((Ps2Ps1 - 1) / (Gamma * M1 ^ 2 - Ps2Ps1 + 1)) ^ 2 * (2 * Gamma * M1 ^ 2 - gm1 - gp1 * Ps2Ps1) / (gp1 * Ps2Ps1 + gm1)) ^ 0.5)

        If AngleIn = AnglesIn.Degrees Then
            Return Delta * 180 / PI
        Else
            Return Delta
        End If

    End Function

    Public Shared Function ShockAngle_fDeflect(ByVal Gamma As Double, ByVal M1 As Double, ByVal Deflect As Double, ByVal AngleIn As AnglesIn, ByVal ShkStr As ShockStrength) As Double
        'calculates the oblique shock angle from deflection of the incoming streamline
        Dim lam, chi, beta, gm1, gp1 As Double
        Dim A, B As Double
        Dim k As Integer
        Dim Def As Double

        If ShkStr = ShockStrength.Weak Then
            k = 1  'weak shock solution
        ElseIf ShkStr = ShockStrength.Strong Then
            k = 0  'strong shock solution
        End If

        If AngleIn = AnglesIn.Degrees Then
            Def = Deflect * PI / 180
        Else
            Def = Deflect
        End If

        If Def < 0.000001 Then
            beta = Asin(1 / M1)
        Else
            gm1 = Gamma - 1
            gp1 = Gamma + 1

            A = (M1 ^ 2 - 1) ^ 2
            B = 3 * (1 + gm1 * M1 ^ 2 / 2) * (1 + gp1 * M1 ^ 2 / 2) * Tan(Def) ^ 2

            If (A - B) < 0 Then
                Return -9999
            Else
                lam = (A - B) ^ 0.5
            End If

            chi = ((M1 ^ 2 - 1) ^ 3 - 9 * (1 + gm1 * M1 ^ 2 / 2) * (1 + gm1 * M1 ^ 2 / 2 + gp1 * M1 ^ 4 / 4) * Tan(Def) ^ 2) / lam ^ 3
            If Abs(chi) > 1 Then
                Return -9999
            End If

            beta = Atan((M1 ^ 2 - 1 + (2 * lam * Cos((4 * PI * k + Acos(chi)) / 3))) / (3 * (1 + gm1 * M1 ^ 2 / 2) * Tan(Def)))
        End If

        If AngleIn = AnglesIn.Degrees Then
            Return beta * 180 / PI
        Else
            Return beta
        End If


    End Function

    Public Shared Function ShockAngle_fP2P1(ByVal Gamma As Double, ByVal M1 As Double, ByVal P2P1 As Double, ByVal AngleIn As AnglesIn) As Double
        'Calculates the shock angle from the static pressure ratio across the shock
        Dim gp1, gm1 As Double
        Dim a, beta As Double

        gp1 = Gamma + 1
        gm1 = Gamma - 1

        a = (gp1 * P2P1 + gm1) / (2 * Gamma * M1 ^ 2)
        beta = Acos(1 - 2 * a) / 2

        If AngleIn = AnglesIn.Degrees Then
            Return beta * 180 / PI
        Else
            Return beta
        End If

    End Function

    Public Shared Function ShockAngle_fP2P1_Old(ByVal Gamma As Double, ByVal M1 As Double, ByVal Ps2Ps1 As Double, ByVal AngleIn As AnglesIn) As Double
        'This function iterates to find the oblique shock angle
        'that will produce the specified pressure rise
        Const tol As Double = 0.00001
        Dim PRmax, Ang, Del, kdir, PR As Double

        PRmax = ShockPs2Ps1(Gamma, M1, 90, AnglesIn.Degrees)
        If Ps2Ps1 > PRmax Then
            Ang = 90
            GoTo 99
        End If
        Del = 5
        Ang = 40
        kdir = 1
10:     PR = ShockPs2Ps1(Gamma, M1, Ang, AnglesIn.Degrees)
        If Abs(PR - Ps2Ps1) / Ps2Ps1 > tol Then
            If PR > Ps2Ps1 Then
                If kdir = 1 Then
                    kdir = -1
                    Del = Del / 2
                End If
            Else
                If kdir = -1 Then
                    kdir = 1
                    Del = Del / 2
                End If
            End If
            Ang = Ang + kdir * Del
            GoTo 10
        End If

99:     If AngleIn = AnglesIn.Degrees Then
            Return Ang
        Else
            Return Ang * PI / 180
        End If

    End Function

    Public Shared Function ShockM2(ByVal Gamma As Double, ByVal M1 As Double, ByVal ShockAngle As Double, ByVal Deflection As Double, ByVal AngleIn As AnglesIn) As Double
        'Find Mach number downstream of shock
        '  ShockAngle is the angle of the Shock (90 degrees for normal shock)
        '  Use "Function Oblique" to calculate oblique shock angle
        '  Deflection is the angle of deflection of  the incoming streamline angle
        'This relationship holds true for oblique shocks by substituting as follows
        '  M1=M1*sin(Oblique angle)
        '  M2=M2*sin(Oblique angle-Wedge angle)
        Dim AngR As Double, WedR As Double, M2norm As Double

        If AngleIn = AnglesIn.Degrees Then
            AngR = ShockAngle * PI / 180
            WedR = Deflection * PI / 180
        Else
            AngR = ShockAngle
            WedR = Deflection
        End If

        M2norm = ((2 + (Gamma - 1) * M1 ^ 2 * Sin(AngR) ^ 2) / _
                  (2 * Gamma * M1 ^ 2 * Sin(AngR) ^ 2 - Gamma + 1)) ^ 0.5

        Return M2norm / Sin(AngR - WedR)

    End Function

    Public Shared Function ShockPs2Ps1(ByVal Gamma As Double, ByVal M1 As Double, ByVal ShockAngle As Double, ByVal AngleIn As AnglesIn) As Double
        'Find static pressure ratio across a shock
        '  M1 is the Mach number upstream of the shock
        '  Angle is the angle of the Shock (90 degrees for normal shock)
        '  Use "Function Oblique" to calculate oblique shock angle

        Dim AngR As Double

        If AngleIn = AnglesIn.Degrees Then
            AngR = ShockAngle * PI / 180
        Else
            AngR = ShockAngle
        End If

        Return (2 * Gamma * M1 ^ 2 * Sin(AngR) ^ 2 - Gamma + 1) / (Gamma + 1)

    End Function

    Public Shared Function ShockPt2Ps1(ByVal Gamma As Double, ByVal M1 As Double, ByVal ShockAngle As Double, ByVal AngleIn As AnglesIn) As Double
        'Find the ratio of downstream total to upstream static pressure ratio across a shock
        '  Angle is the angle of the Shock (90 degrees for normal shock)
        '  Use "Function Oblique" to calculate oblique shock angle
        Dim AngR As Double

        If AngleIn = AnglesIn.Degrees Then
            AngR = ShockAngle * PI / 180
        Else
            AngR = ShockAngle
        End If

        If AngR = PI / 2 Then
            Return ((Gamma + 1) * M1 ^ 2 * Sin(AngR) ^ 2 / 2) ^ (Gamma / (Gamma - 1)) _
                 * ((Gamma + 1) / (2 * Gamma * M1 ^ 2 * Sin(AngR) ^ 2 - Gamma + 1)) ^ (1 / (Gamma - 1))
        Else
            Return ((Gamma + 1) / (2 * Gamma * M1 ^ 2 * Sin(AngR) ^ 2 - Gamma + 1)) _
                ^ (1 / (Gamma - 1)) * _
                ((Gamma + 1) * M1 ^ 2 * Sin(AngR) ^ 2 * ((Gamma - 1) * M1 ^ 2 + 2) / 2 / _
                ((Gamma - 1) * M1 ^ 2 * Sin(AngR) ^ 2 + 2)) ^ (Gamma / (Gamma - 1))
        End If

    End Function

    Public Shared Function ShockPt2Pt1(ByVal Gamma As Double, ByVal M1 As Double, ByVal ShockAngle As Double, ByVal AngleIn As AnglesIn) As Double
        'Find total pressure ratio across a shock
        '  Angle is the angle of the Shock (90 degrees for normal shock)
        '  Use "Function Oblique" to calculate oblique shock angle

        Dim AngR As Double

        If AngleIn = AnglesIn.Degrees Then
            AngR = ShockAngle * PI / 180
        Else
            AngR = ShockAngle
        End If


        Return ((Gamma + 1) * M1 ^ 2 * Sin(AngR) ^ 2 / _
                ((Gamma - 1) * M1 ^ 2 * Sin(AngR) ^ 2 + 2)) ^ (Gamma / (Gamma - 1)) _
                * ((Gamma + 1) / (2 * Gamma * M1 ^ 2 * Sin(AngR) ^ 2 - Gamma + 1)) ^ (1 / (Gamma - 1))

    End Function

    Public Shared Function ShockPs2Pt1(ByVal Gamma As Double, ByVal M1 As Double, ByVal ShockAngle As Double, ByVal AngleIn As AnglesIn) As Double
        'Find ratio of downstream static to upstream total pressure across shock
        '  Angle is the angle of the Shock (90 degrees for normal shock)
        '  Use "Function Oblique" to calculate oblique shock angle

        Dim AngR As Double

        If AngleIn = AnglesIn.Degrees Then
            AngR = ShockAngle * PI / 180
        Else
            AngR = ShockAngle
        End If

        Return (2 * Gamma * M1 ^ 2 * Sin(AngR) ^ 2 - Gamma + 1) / (Gamma + 1) * _
                  (2 / ((Gamma - 1) * M1 ^ 2 + 2)) ^ (Gamma / (Gamma - 1))

    End Function

    Public Shared Function ShockPs2Pt2(ByVal gamma As Double, ByVal m1 As Double, ByVal ShockAngle As Double, ByVal AngleIn As AnglesIn) As Double
        'Find static-to-total pressure ratio downstream of shock
        '  Angle is the angle of the Shock (90 degrees for normal shock)
        '  Use "Function Oblique" to calculate oblique shock angle

        Dim AngR As Double

        If AngleIn = AnglesIn.Degrees Then
            AngR = ShockAngle * PI / 180
        Else
            AngR = ShockAngle
        End If

        If AngR = PI / 2 Then
            Return ((4 * gamma * m1 ^ 2 - 2 * (gamma - 1)) / _
                    ((gamma + 1) ^ 2 * m1 ^ 2)) ^ (gamma / (gamma - 1))
        Else
            Return ((4 * gamma * m1 ^ 2 * Sin(AngR) ^ 2 - 2 * (gamma - 1)) * _
                    ((gamma - 1) * m1 ^ 2 * Sin(AngR) ^ 2 + 2) / _
                    ((gamma + 1) ^ 2 * m1 ^ 2 * Sin(AngR) ^ 2) / _
                    ((gamma - 1) * m1 ^ 2 + 2)) ^ (gamma / (gamma - 1))
        End If

    End Function

    Public Shared Function ShockTs2Ts1(ByVal Gamma As Double, ByVal M1 As Double, ByVal ShockAngle As Double, ByVal AngleIn As AnglesIn) As Double
        'Find static temperature ratio across shock
        '  Angle is the angle of the Shock (90 degrees for normal shock)
        '  Use "Function Oblique" to calculate oblique shock angle

        Dim AngR As Double

        If AngleIn = AnglesIn.Degrees Then
            AngR = ShockAngle * PI / 180
        Else
            AngR = ShockAngle
        End If

        Return (2 * Gamma * M1 ^ 2 * Sin(AngR) ^ 2 - Gamma + 1) * _
                ((Gamma - 1) * M1 ^ 2 * Sin(AngR) ^ 2 + 2) / ((Gamma + 1) ^ 2 * M1 ^ 2 * Sin(AngR) ^ 2)

    End Function

End Class
