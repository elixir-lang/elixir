IO.puts(1)

defmodule ElixirPatternIssue do
  def process(data, _opts) do
    case data do
      %{status: :pending} -> :ok
      %{status: :completed} -> :ok
      %{category: :alpha, kind: :primary, status: status} -> status
      %{category: :alpha, kind: :secondary, status: status} -> status
      %{category: :beta, kind: :primary, status: status} -> status
      %{category: :beta, kind: :secondary, status: status} -> status
      %{category: :gamma, kind: :primary, status: status} -> status
      %{category: :gamma, kind: :secondary, status: status} -> status
      %{category: :delta, kind: :primary, status: status} -> status
      %{category: :delta, kind: :secondary, status: status} -> status
      %{category: :epsilon, kind: :primary, status: status} -> status
      %{category: :epsilon, kind: :secondary, status: status} -> status
      %{category: :zeta, kind: :primary, status: status} -> status
      %{category: :zeta, kind: :secondary, status: status} -> status
      %{category: :eta, kind: :primary, status: status} -> status
      %{category: :eta, kind: :secondary, status: status} -> status
      %{category: :theta, kind: :primary, status: status} -> status
      %{category: :theta, kind: :secondary, status: status} -> status
      %{category: :iota, kind: :primary, status: status} -> status
      %{category: :iota, kind: :secondary, status: status} -> status
      %{category: :kappa, kind: :primary, status: status} -> status
      %{category: :kappa, kind: :secondary, status: status} -> status
      %{category: :lambda, kind: :primary, status: status} -> status
      %{category: :lambda, kind: :secondary, status: status} -> status
      %{category: :mu, kind: :primary, status: status} -> status
      %{category: :mu, kind: :secondary, status: status} -> status
      %{category: :nu, kind: :primary, status: status} -> status
      %{category: :nu, kind: :secondary, status: status} -> status
      %{category: :xi, kind: :primary, status: status} -> status
      %{category: :xi, kind: :secondary, status: status} -> status
      %{category: :omicron, kind: :primary, status: status} -> status
      %{category: :omicron, kind: :secondary, status: status} -> status
      %{category: :pi, kind: :primary, status: status} -> status
      %{category: :pi, kind: :secondary, status: status} -> status
      %{category: :rho, kind: :primary, status: status} -> status
      %{category: :rho, kind: :secondary, status: status} -> status
      %{category: :sigma, kind: :primary, status: status} -> status
      %{category: :sigma, kind: :secondary, status: status} -> status
      %{category: :tau, kind: :primary, status: status} -> status
      %{category: :tau, kind: :secondary, status: status} -> status
      %{category: :upsilon, kind: :primary, status: status} -> status
      %{category: :upsilon, kind: :secondary, status: status} -> status
      %{category: :phi, kind: :primary, status: status} -> status
      %{category: :phi, kind: :secondary, status: status} -> status
      %{category: :chi, kind: :primary, status: status} -> status
      %{category: :chi, kind: :secondary, status: status} -> status
      %{category: :psi, kind: :primary, status: status} -> status
      %{category: :psi, kind: :secondary, status: status} -> status
      %{category: :omega, kind: :primary, status: status} -> status
      %{category: :omega, kind: :secondary, status: status} -> status
      %{category: :item, kind: :type_a, variant: nil, status: status} -> status
      %{category: :item, kind: :type_b, variant: nil, status: status} -> status
      %{category: :item, kind: :type_c, variant: nil, status: status} -> status
      %{category: :item, kind: :type_d, variant: nil, status: status} -> status
      %{category: :item, kind: :type_e, variant: nil, status: status} -> status
      %{category: :item, kind: :type_f, variant: nil, status: status} -> status
      %{category: :item, kind: :type_g, variant: nil, status: status} -> status
      %{category: :item, kind: :type_h, variant: nil, status: status} -> status
      %{category: :item, kind: :type_i, variant: nil, status: status} -> status
      %{category: :item, kind: :type_j, variant: nil, status: status} -> status
      %{category: :item, kind: :type_k, variant: nil, status: status} -> status
      %{category: :item, kind: :type_l, variant: nil, status: status} -> status
      %{category: :item, kind: :type_m, variant: nil, status: status} -> status
      %{category: :item, kind: :type_n, variant: nil, status: status} -> status
      %{category: :item, kind: :type_o, variant: nil, status: status} -> status
      %{category: :item, kind: :type_p, variant: nil, status: status} -> status
      %{category: :item, kind: :type_q, variant: nil, status: status} -> status
      %{category: :item, kind: :type_r, variant: nil, status: status} -> status
      %{category: :item, kind: :type_s, variant: nil, status: status} -> status
      %{category: :item, kind: :type_t, variant: nil, status: status} -> status
      %{category: :item, kind: :type_u, variant: nil, status: status} -> status
      %{category: :item, kind: :type_v, variant: nil, status: status} -> status
      %{category: :item, kind: :type_w, variant: nil, status: status} -> status
      %{category: :item, kind: :type_x, variant: nil, status: status} -> status
      %{category: :item, kind: :type_y, variant: nil, status: status} -> status
      %{category: :item, kind: :type_z, variant: nil, status: status} -> status
      %{category: :item, kind: :type_aa, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ab, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ac, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ad, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ae, variant: nil, status: status} -> status
      %{category: :item, kind: :type_af, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ag, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ah, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ai, variant: nil, status: status} -> status
      %{category: :item, kind: :type_aj, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ak, variant: nil, status: status} -> status
      %{category: :item, kind: :type_al, variant: nil, status: status} -> status
      %{category: :item, kind: :type_am, variant: nil, status: status} -> status
      %{category: :item, kind: :type_an, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ao, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ap, variant: nil, status: status} -> status
      %{category: :item, kind: :type_aq, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ar, variant: nil, status: status} -> status
      %{category: :item, kind: :type_as, variant: nil, status: status} -> status
      %{category: :item, kind: :type_at, variant: nil, status: status} -> status
      %{category: :item, kind: :type_au, variant: nil, status: status} -> status
      %{category: :item, kind: :type_av, variant: nil, status: status} -> status
      %{category: :item, kind: :type_aw, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ax, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ay, variant: nil, status: status} -> status
      %{category: :item, kind: :type_az, variant: nil, status: status} -> status
      %{category: :item, kind: :type_ba, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bb, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bc, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bd, variant: nil, status: status} -> status
      %{category: :item, kind: :type_be, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bf, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bg, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bh, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bi, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bj, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bk, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bl, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bm, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bn, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bo, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bp, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bq, variant: nil, status: status} -> status
      %{category: :item, kind: :type_br, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bs, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bt, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bu, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bv, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bw, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bx, variant: nil, status: status} -> status
      %{category: :item, kind: :type_by, variant: nil, status: status} -> status
      %{category: :item, kind: :type_bz, variant: nil, status: status} -> status
      _ -> :error
    end
  end
end

IO.puts(2)

defmodule ElixirPatternIssue2 |> IO.inspect() do
  def process(%{status: :pending}, _opts), do: :ok
  def process(%{category: :alpha, kind: :primary, status: status}, _opts), do: status
  def process(%{category: :alpha, kind: :secondary, status: status}, _opts), do: status
  def process(%{category: :beta, kind: :primary, status: status}, _opts), do: status
  def process(%{category: :beta, kind: :secondary, status: status}, _opts), do: status
  def process(%{category: :gamma, kind: :primary, status: status}, _opts), do: status
  def process(%{category: :gamma, kind: :secondary, status: status}, _opts), do: status
  def process(%{category: :delta, kind: :primary, status: status}, _opts), do: status
  def process(%{category: :delta, kind: :secondary, status: status}, _opts), do: status
  def process(%{category: :epsilon, kind: :primary, status: status}, _opts), do: status
  def process(%{category: :epsilon, kind: :secondary, status: status}, _opts), do: status
  def process(%{category: :zeta, kind: :primary, status: status}, _opts), do: status
  def process(%{category: :zeta, kind: :secondary, status: status}, _opts), do: status
  def process(%{category: :eta, kind: :primary, status: status}, _opts), do: status
  def process(%{category: :eta, kind: :secondary, status: status}, _opts), do: status
  def process(%{category: :theta, kind: :primary, status: status}, _opts), do: status
  def process(%{category: :theta, kind: :secondary, status: status}, _opts), do: status
  def process(%{category: :iota, kind: :primary, status: status}, _opts), do: status
  def process(%{category: :iota, kind: :secondary, status: status}, _opts), do: status
  def process(%{category: :kappa, kind: :primary, status: status}, _opts), do: status
end

IO.puts(3)

defmodule ElixirPatternIssue3 |> IO.inspect() do
  def process(%{status: :pending}, _opts), do: {:ok, "Pending"}
  def process(%{status: :completed}, _opts), do: {:ok, "Completed"}

  def process(%{category: :alpha, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :alpha, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :beta, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :beta, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :gamma, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :gamma, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :delta, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :delta, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :epsilon, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :epsilon, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :zeta, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :zeta, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :eta, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :eta, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :theta, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :theta, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :iota, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :iota, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :kappa, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :kappa, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :lambda, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :lambda, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :mu, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :mu, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :nu, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :nu, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :xi, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :xi, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :omicron, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :omicron, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :pi, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :pi, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :rho, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :rho, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :sigma, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :sigma, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :tau, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :tau, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :upsilon, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :upsilon, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :phi, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :phi, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :chi, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :chi, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :psi, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :psi, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :omega, kind: :primary} = data, _opts), do: {:ok, "#{data.status}"}
  def process(%{category: :omega, kind: :secondary} = data, _opts), do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_a, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_b, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_c, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_d, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_e, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_f, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_g, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_h, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_i, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_j, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_k, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_l, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_m, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_n, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_o, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_p, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_q, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_r, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_s, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_t, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_u, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_v, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_w, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_x, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_y, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_z, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_aa, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ab, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ac, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ad, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ae, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_af, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ag, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ah, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ai, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_aj, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ak, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_al, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_am, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_an, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ao, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ap, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_aq, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ar, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_as, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_at, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_au, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_av, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_aw, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ax, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ay, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_az, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_ba, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bb, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bc, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bd, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_be, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bf, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bg, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bh, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bi, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bj, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bk, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bl, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bm, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bn, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bo, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bp, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bq, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_br, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bs, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bt, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bu, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bv, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bw, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bx, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_by, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(%{category: :item, kind: :type_bz, variant: nil} = data, _opts),
    do: {:ok, "#{data.status}"}

  def process(_data, _opts), do: {:error, :unsupported}
end
